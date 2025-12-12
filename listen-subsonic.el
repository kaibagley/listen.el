;;; listen-subsonic.el --- Subsonic server support for listen.el         -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Free Software Foundation, Inc.

;; Author: Kai Bagley <kaibagley@proton.mail>
;; Maintainer: Kai Bagley <kaibagley@proton.mail>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; * OpenSubsonic API Implementation
;; ** 1.0.0
;; - [ ] download
;; - [ ] getCoverArt
;; - [ ] getIndexes
;; - [ ] getLicense
;; - [ ] getMusicDirectory
;; - [ ] getMusicFolders
;; - [ ] getNowPlaying
;; - [X] getPlaylist
;; - [X] getPlaylists
;; - [X] ping
;; - [ ] search
;; - [X] stream
;; ** 1.1.0
;; - [ ] changePassword
;; - [ ] createUser
;; ** 1.2.0
;; - [ ] addChatMessage
;; - [ ] createPlaylist
;; - [ ] deletePlaylist
;; - [ ] getAlbumList
;; - [ ] getChatMessages
;; - [ ] getLyrics
;; - [X] getRandomSongs
;; - [ ] jukeboxControl
;; ** 1.3.0
;; - [ ] deleteUser
;; - [ ] getUser
;; ** 1.4.0
;; - [ ] search2
;; ** 1.5.0
;; - [X] scrobble
;; ** 1.6.0
;; - [ ] createShare
;; - [ ] deleteShare
;; - [ ] getPodcasts
;; - [ ] getShares
;; - [X] setRating
;; - [ ] updateShare
;; ** 1.8.0
;; - [X] getAlbum
;; - [ ] getAlbumList2
;; - [X] getArtist
;; - [X] getArtists
;; - [ ] getAvatar
;; - [ ] getSong
;; - [ ] getStarred
;; - [X] getStarred2
;; - [ ] getUsers
;; - [ ] getVideos
;; - [ ] hls
;; - [X] search3
;; - [X] star
;; - [X] unstar
;; - [ ] updatePlaylist
;; ** 1.9.0
;; - [ ] createBookmark
;; - [ ] createPodcastChannel
;; - [ ] deleteBookmark
;; - [ ] deletePodcastChannel
;; - [ ] deletePodcastEpisode
;; - [ ] downloadPodcastEpisode
;; - [ ] getBookmarks
;; - [ ] getGenres
;; - [ ] getInternetRadioStations
;; - [ ] getSongsByGenre
;; - [ ] refreshPodcasts
;; ** 1.10.1
;; - [ ] updateUser
;; ** 1.11.0
;; - [ ] getArtistInfo
;; - [ ] getArtistInfo2
;; - [ ] getSimilarSongs
;; - [ ] getSimilarSongs2
;; ** 1.12.0(100.0%)
;; - [ ] getPlayQueue
;; - [ ] savePlayQueue
;; ** 1.13.0
;; - [ ] getNewestPodcasts
;; - [ ] getTopSongs
;; ** 1.14.0
;; - [ ] getAlbumInfo
;; - [ ] getAlbumInfo2
;; - [ ] getCaptions
;; - [ ] getVideoInfo
;; ** 1.15.0
;; - [ ] getScanStatus
;; - [ ] startScan
;; ** 1.16.0
;; - [ ] createInternetRadioStation
;; - [ ] deleteInternetRadioStation
;; - [ ] updateInternetRadioStation

;;

;;; Code:

;;;; Requirements

;; TODO: Some kind of indicator to show if track is starred or not
;; TODO: Send bookmark request to server periodically
;; TODO: When emacs 31.1 is released, cl-decf/cl-incf -> decf/incf

(require 'plz)          ; HTTP requests
(require 'auth-source)  ; authinfo
(require 'listen-queue) ; Add tracks to queue
(require 'svg-lib)      ; For starred icon

(require 'map)          ; for map-let and map-elt
(require 'url-util)     ; for url-build-query-string

;; Declares

(declare-function listen-library "listen-library")

;;;; Customisation

(defgroup listen-subsonic nil
  "`listen' options for Subsonic backend."
  :group 'listen)

(defcustom listen-subsonic-url nil
  "The fully-qualified domain name of your Subsonic-compatible server.
For example, \"music.example.com\" or \"192.168.0.0:4533\".
Don't include the procol/scheme or the resource path."
  :type 'string
  :group 'listen-subsonic)

(defcustom listen-subsonic-protocol "https"
  "Protocol to use for calls to Subsonic API.
Must be either \"http\" or \"https\" (default)."
  :type '(choice (const :tag "HTTPS" "https")
                 (const :tag "HTTP" "http"))
  :group 'listen-subsonic)

(defcustom listen-subsonic-search-max-results 200
  "Maximum results to return in search queries."
  :type 'integer
  :group 'listen-subsonic)

(defcustom listen-subsonic-user-agent "listen.el"
  "User-agent used in API requests.
Used by the server to identify `listen'."
  :type 'string
  :group 'listen-subsonic)

(defface listen-starred
  '((t :inherit font-lock-warning-face))
  "Face for starred Subsonic tracks."
  :group 'listen-subsonic)

(defvar listen-subsonic-cache-dir (expand-file-name "listen.el" temporary-file-directory)
  "Directory to store cached files such as cover art.")

(defvar listen-subsonic--art-queue nil
  "Queue for art downloads in browser.
Each element is a list: (url filename buffer position).
url is the URL of the art to download.
filename is the file to write to.
buffer and position specify where to display the art when downloaded.")

(defvar listen-subsonic--art-active 0
  "Number of concurrent active downloads associated with `listen-subsonic--art-queue'.
Used to keep concurrent downloads below `listen-subsonic--art-max'.")

(defvar listen-subsonic--art-max 10
  "Max allowed concurrent downloads.
Used to limit connections to the server.")

;;;; General helpers

;;;; Auth helpers

(defun listen-subsonic--get-credentials ()
  "Fetch user credentials securely using `auth-source'.
Returns an auth-source plist, or nil if not found.

Searches `auth-source' files for an entry with \":host\" matching `listen-subsonic-url'."
  (or (car (auth-source-search :host listen-subsonic-url)) nil))

(defun listen-subsonic--get-auth-params ()
  "Return authentication info for Subsonic API calls.
Return an alist of strings: ((\"u\" . \"myusername\") (\"t\" . \"<randomstring>\") ...)."
  (let* ((creds (listen-subsonic--get-credentials))
         (user (plist-get creds :user))
         (pass (funcall (plist-get creds :secret)))
         (salt (format "%06x" (random #xffffff)))
         (token (md5 (concat pass salt))))
    `(("u" . ,user)
      ("t" . ,token)
      ("s" . ,salt)
      ("v" . "1.16.1")
      ("c" . ,listen-subsonic-user-agent)
      ("f" . "json"))))

(defun listen-subsonic--build-url (endpoint params)
  "Build a Subsonic REST API URL from ENDPOINT and PARAMS.
Returns a complete URL required to make an API call.

ENDPOINT is the API method name, see `https://www.navidrome.org/docs/developers/subsonic-api/' for
details.
PARAMS is an alist of query parameters."
  (let* ((param-list (mapcar (lambda (p)
                               (list (car p) (cdr p)))
                             params))
         (param-str (url-build-query-string param-list nil t)))
    (format "%s://%s/rest/%s.view?%s"
            listen-subsonic-protocol
            listen-subsonic-url
            endpoint
            param-str)))

;;;; API Helpers

(defun listen-subsonic--process-api-response ()
  "Parse JSON response from a Subsonic API request.
Returns data contained in `subsonic-response' alist, or signals an error.

Should be called from a buffer containing an API response."
  (goto-char (point-min))
  (when (zerop (buffer-size))
    (error "Subsonic API response is empty"))
  (let* ((json-data (json-parse-buffer :object-type 'alist
                                       :null-object nil
                                       :false-object nil
                                       :array-type 'list))
         (response (alist-get 'subsonic-response json-data)))
    (unless (string-equal "ok" (alist-get 'status response))
      (error "Subsonic API response returned error: %s"
             (alist-get 'message (alist-get 'error response))))
    response))

(defun listen-subsonic--api-call (endpoint &optional params callback)
  "Make a call to the Subsonic API.
Returns the parsed JSON if CALLBACK is nil.
Returns the curl process object if CALLBACK is non-nil.

ENDPOINT is the API method name, see `https://www.navidrome.org/docs/developers/subsonic-api/' for
details.
PARAMS is an alist of additional parameters.
If CALLBACK is nil, run synchronously and parse the JSON response.
If CALLBACK is non-nil, run asynchronously and parse the JSON response, then call CALLBACK on the
parsed JSON.

The JSON should usually be processed by `listen-subsonic--process-api-response'."
  (unless listen-subsonic-url
    (user-error "Please set `listen-subsonic-url'"))
  (let* ((api-params (append (listen-subsonic--get-auth-params) params))
         (api-url (listen-subsonic--build-url endpoint api-params))
         (api-headers '(("Accept-Encoding" . "gzip"))))
    (plz 'get api-url
      :headers api-headers
      :as #'listen-subsonic--process-api-response
      :then (or callback 'sync)
      :else (lambda (plz-err)
              (message "Subsonic API request error: %s"
                       (status (plz-response-status (plz-error-response err))))))))

;;;; Data formatting

(defun listen-subsonic--get-stream-url (id &optional auth-params)
  "Create a streaming URL for track with ID.
Returns a complete URL for MPV or VLC to directly stream from the server.

If AUTH-PARAMS is nil, new auth params are generated."
  (listen-subsonic--build-url
   "stream"
   (append (or auth-params (listen-subsonic--get-auth-params))
           `(("id" . ,id)))))

(defun listen-subsonic--json-to-listen (json-data &optional auth-params)
  "Convert Subsonic JSON-DATA into a `listen-track'.
Returns a `listen-track' struct.

If AUTH-PARAMS is nil, new auth params are generated."
  (map-let
      (('id id) ('userRating rating) artist title album track genre duration year starred)
      json-data
    (make-listen-track
     :filename (listen-subsonic--get-stream-url id auth-params) ; silly mpv
     :artist artist
     :title title
     :album album
     :number (number-to-string (or track 0))
     :genre genre
     :duration (or duration 0)
     :date year
     ;; Rating is a string, "0.0" - "1.0". Subsonic returns 0-5 or nil
     :rating (when rating (format "%f" (/ rating 5.0)))
     :metadata json-data
     :etc `((source . "subsonic")
            (id . ,id)
            (starred . ,(when starred t))))))

;;;; Read requests

(defun listen-subsonic--get-items (endpoint rootkey itemkey &optional params)
  "Get data from ENDPOINT and extract the contents of ROOTKEY, then ITEMKEY.
Returns a list of alists representing the items.

ENDPOINT is the API method name, see `https://www.navidrome.org/docs/developers/subsonic-api/' for
details.
ROOTKEY is the top-level JSON key in the API repsonse, ITEMKEY is the
inner key (for example, \"searchResult3\" and \"song\"). Go to the above link for details.
PARAMS are optional API parameters."
  (let* ((response (listen-subsonic--api-call endpoint params))
         (root (alist-get rootkey response))
         (items (alist-get itemkey root)))
    items))

(defun listen-subsonic-search-tracks (query)
  "Search the server for tracks matching QUERY.
Returns a list of `listen-track's.

Uses the Subsonic API's \"search3\" endpoint with QUERY as the search query."
  (let ((items (listen-subsonic--get-items
                "search3" 'searchResult3 'song
                `(("query" . ,query)
                  ("songCount" . ,(number-to-string listen-subsonic-search-max-results)))))
        (auth (listen-subsonic--get-auth-params)))
    (mapcar (lambda (item)
              (listen-subsonic--json-to-listen item auth))
            items)))

(defun listen-subsonic-get-starred-tracks ()
  "Fetch all starred songs from the server.
Returns a list of `listen-track's."
  (let ((items (listen-subsonic--get-items
                "getStarred2" 'starred2 'song))
        (auth (listen-subsonic--get-auth-params)))
    (mapcar (lambda (item)
              (listen-subsonic--json-to-listen item auth))
            items)))

(defun listen-subsonic--get-playlists ()
  "Fetch all of the user's playlists from the server.
Returns an alist mapping playlist names to their IDs: ((name . id) ...)."
  (let ((items (listen-subsonic--get-items
                "getPlaylists" 'playlists 'playlist)))
    (mapcar (lambda (item)
              (cons (alist-get 'name item)
                    (format "%s" (alist-get 'id item))))
            items)))

(defun listen-subsonic--get-playlist-tracks (id)
  "Fetch all tracks in playlist with ID.
Returns a list of `listen-track's."
  (let ((items (listen-subsonic--get-items
                "getPlaylist" 'playlist 'entry
                `(("id" . ,id))))
        (auth (listen-subsonic--get-auth-params)))
    (mapcar (lambda (item)
              (listen-subsonic--json-to-listen item auth))
            items)))

(defun listen-subsonic--get-all-tracks (id &optional level auth)
  "Fetch all tracks under item associated with ID.
Returns a list of `listen-track's.

LEVEL determines what level of the hierarchy we are on:
- :artist: fetches all albums, then all songs by that artist.
- :album: fetches all songs on the album."
  (let ((auth (or auth (listen-subsonic--get-auth-params))))
    (pcase level
      (:artist
       (let* ((data (listen-subsonic--api-call "getArtist" `(("id" . ,id))))
              (albums (map-nested-elt data '(artist album))))
         (mapcan (lambda (album)
                   (listen-subsonic--get-all-tracks (alist-get 'id album) :album auth))
                 albums)))
      (:album
       (let* ((data (listen-subsonic--api-call "getAlbum" `(("id" . ,id))))
              (tracks (map-nested-elt data '(album song))))
         (mapcar (lambda (track)
                   (listen-subsonic--json-to-listen track auth))
                 tracks))))))

;;;; Write requests

(defun listen-subsonic--star-item (id star-p &optional callback)
  "Set ID's (artist, album or track) star status according to STAR-P.
Returns the unparsed API response.

Send a request to the \"star\" or \"unstar\" Subsonic endpoints, star (when STAR-P is non-nil) or
unstar ID. CALLBACK is passed to `listen-subsonic--api-call' and is evaluated on the response data.

This function does not set the corresponding item's star status locally. Perhaps use CALLBACK for
this."
  (listen-subsonic--api-call (if star-p "star" "unstar")
                             `(("id" . ,id))
                             callback))

(defun listen-subsonic--scrobble (player submission-p)
  "Scrobble the current track playing in PLAYER's queue to the Subsonic API.
Returns the unparsed API response.

When SUBMISSION-P is non-nil, server is notified that the currently playing track is finished.
When SUBMISSION-P is nil, server is notified the current tracks is \"now playing\"."
  (when-let* ((queue (map-elt (listen-player-etc player) :queue))
              (track (listen-queue-current queue))
              (source (equal (map-elt (listen-track-etc track) 'source) "subsonic"))
              (id (alist-get 'id (listen-track-etc track))))
    (let* ((params `(("id". ,id)
                     ("submission" . ,(if submission-p "true" "false")))))
      (listen-subsonic--api-call "scrobble" params #'ignore))))

(defun listen-subsonic-scrobble-start (player)
  "Notifies the server that we have started playing a track in PLAYER.
Should be added to `listen-track-start-functions'."
  (listen-subsonic--scrobble player nil))

(defun listen-subsonic-scrobble-end (player)
  "Notifies the server that we have finished a track in PLAYER.
Should be added to `listen-track-end-functions'."
  (listen-subsonic--scrobble player t))

;;;; Server browsing functions

(defun listen-subsonic--get-nodes (level id)
  "Fetch \"nodes\" for directory hierarchy LEVEL and ID.
Returns a list of alists, each alist representing children of ID.

LEVEL determines the endpoint to use, and may be one of:
- :artists: Returns top-level view of all artists using endpoint \"getArtists\".
- :artist: Returns albums for an artist using \"getArtist\".
- :album: Returns songs in an album using \"getAlbum\"."
  (let ((items
         (pcase level
           (:artists
            (let* ((data (listen-subsonic--api-call "getArtists"))
                   (indexes (map-nested-elt data '(artists index))))
              ;; res is organised alphabetically, so we have to flatten
              (mapcan (lambda (idx)
                        (let ((artists (alist-get 'artist idx)))
                          (mapcar (lambda (artist) (cons '(isDir . t) artist)) artists)))
                      indexes)))
           (:artist
            (let* ((data (listen-subsonic--api-call "getArtist" `(("id" . ,id))))
                   (albums (map-nested-elt data '(artist album))))
              (mapcar (lambda (album) (cons '(isDir . t) album)) albums)))
           (:album
            (let* ((data (listen-subsonic--api-call "getAlbum" `(("id" . ,id))))
                   (tracks (map-nested-elt data '(album song))))
              ;; getAlbum tracks dont have "name", the other 2 endpoints do
              (mapcar (lambda (track)
                        (cons (cons 'name (alist-get 'title track)) track))
                      tracks))))))
    items))

(defun listen-subsonic--browser-next-level (level)
  "Determines the hierarchical level under LEVEL.
Returns the keyword symbol for the next level.

Hierarchy is: :artists -> :artist -> :album."
  (pcase level
    (:artists :artist)
    (:artist :album)
    (_ :album)))

(defun listen-subsonic--dired-get-prefix (item)
  "Create a fixed-width string of `ls'-like metadata for ITEM.
Returns a formatted string of length up to 15 characters.

Example returns:
- Starred song: \"- * 2004 3:43\"
- Artist:       \"d - ---- ----\"
- Album:        \"d - 2004 ----\""
  (let* ((dirp (alist-get 'isDir item))
         (year (alist-get 'year item))
         (duration (alist-get 'duration item))
         (starred (alist-get 'starred item))
         (bitrate (alist-get 'bitRate item)))
    (format "%s %s %4s %5s "
            (if dirp "d" "-")
            (if starred "*" "-")
            (if year (number-to-string year) "----")
            (if dirp "--:--" (listen-format-seconds duration)))))

;;;; Interactive functions

(defun listen-subsonic-ping-server ()
  "Ping the server to check connectivity and authentication.
Returns nil, only displaying a success or failure message."
  (interactive)
  (if (listen-subsonic--api-call "ping")
      (message "Successfully pinged Subsonic server!")
    (message "Failed to ping server.")))

(defun listen-subsonic-star-track (track star-p)
  "Set TRACK's star status according to STAR-P.
Returns the unparsed API response.

When called interactively, the star-state of the currently playing track will be toggled.
Send a request to the \"star\" or \"unstar\" Subsonic endpoints, star (when STAR-P is non-nil) or
unstar TRACK.

This function also sets TRACK's in-memory star status accordingly."
  (interactive
   (let ((track (listen-current-track)))
     (unless track
       (user-error "No track playing."))
     (list track (not (alist-get 'starred (listen-track-etc track))))))
  (when-let* ((id (alist-get 'id (listen-track-etc track))))
    (listen-subsonic--star-item id
                                star-p
                                ;; update track in-memory
                                (lambda (_)
                                  (setf (alist-get 'starred (listen-track-etc track)) star-p)
                                  (message "%s '%s'" (if star-p "Starred" "Unstarred")
                                           (listen-track-title track))))))

(defun listen-subsonic--read-playlist ()
  "Prompt user to select a Subsonic playlist using `completing-read'.
Returns the selected playlist's ID as a string."
  (let* ((playlists (listen-subsonic--get-playlists))
         (name (completing-read "Playlist: " playlists nil t)))
    (alist-get name playlists nil nil #'equal)))

(defun listen-subsonic--affixation (hashtable suffix-fn &optional face)
  "Create an affixation function for `completing-read' candidates in HASHTABLE.
Returns an affixation function which maps a list of candidates to a list of suffixes.

SUFFIX-FN returns the suffix string from the object found in HASHTABLE.
FACE is applied to the suffix."
  (lambda (cands)
    (mapcar (lambda (cand)
              (let* ((item (gethash cand hashtable))
                     (len (string-width cand))
                     (padding (make-string (max 5 (- 40 len)) ?\s))
                     (suffix (or (funcall suffix-fn item) "")))
                (list cand
                      ""
                      (concat padding (propertize suffix 'face face)))))
            cands)))

(defun listen-subsonic--suffix-track (track)
  "Return TRACK's album name to be used as an `affixation-function' suffix."
  (listen-track-album track))

(defun listen-subsonic--suffix-playlist (playlist)
  "Returns PLAYLIST's song count to be used as an `affixation-function' suffix."
  (concat (number-to-string (or (alist-get 'songCount playlist) 0)) " tracks"))

(defun listen-subsonic--suffix-node (node)
  "Returns affixation suffix for NODE.

Displays year for directories and albums, and duration for songs."
  (cond
   ;; ".." and "[All]"
   ((symbolp node) "")
   ;; album or folder
   ((alist-get 'isDir node)
    (if-let ((year (alist-get 'year node)))
        (number-to-string year)
      ""))
   ;; song
   (t (listen-format-seconds (or (alist-get 'duration node) 0)))))

(defun listen-subsonic--read-track (tracks prompt)
  "Prompt user to select a track from TRACKS, displaying PROMPT.
Returns the selected track as a `listen-track'.

Handles duplicate names by appending a counter."
  (let ((track-map (make-hash-table :test 'equal)))
    (dolist (track tracks)
      ;; use "artist - track" as id
      (let* ((artist-track (format "%s - %s"
                                   (propertize (listen-track-artist track) 'face 'listen-artist)
                                   (propertize (listen-track-title track) 'face 'listen-title)))
             (name artist-track)
             (count 1))
        ;; add number to duplicates
        (while (gethash name track-map)
          (cl-incf count)
          (setq name (format "%s %s"
                             artist-track
                             (propertize (format "(%d)" count) 'face 'shadow))))
        (puthash name track track-map)))
    (let* ((completion-extra-properties
            `(:affixation-function
              ,(listen-subsonic--affixation track-map
                                            #'listen-subsonic--suffix-track
                                            'listen-album)))
           (selected-name (completing-read prompt track-map nil t)))
      (gethash selected-name track-map))))

(defun listen-subsonic-queue-random (n queue)
  "Fetch N random songs from the server and add them to QUEUE."
  (interactive
   (list
    (read-number "Number of songs: " 10)
    (listen-queue-complete :allow-new-p t)))
  (let* ((items (listen-subsonic--get-items
                 "getRandomSongs" 'randomSongs 'song
                 `(("size" . ,(number-to-string n)))))
         (auth (listen-subsonic--get-auth-params))
         (tracks (mapcar (lambda (item)
                           (listen-subsonic--json-to-listen item auth))
                         items)))
    (listen-queue-add-tracks tracks queue)))

(defun listen-subsonic-queue-playlist (queue)
  "Prompt for a playlist and add its tracks to QUEUE."
  (interactive (list (listen-queue-complete :allow-new-p t)))
  (let* ((id (listen-subsonic--read-playlist))
         (tracks (listen-subsonic--get-playlist-tracks id)))
    (listen-queue-add-tracks tracks queue)))

(defun listen-subsonic-queue-starred-tracks (queue)
  "Fetch all starred tracks and add them to QUEUE."
  (interactive (list (listen-queue-complete :allow-new-p t)))
  (let ((tracks (listen-subsonic-get-starred-tracks)))
    (listen-queue-add-tracks tracks queue)))

;; TODO: C-u adds to start of queue/next? Waiting for listen-queue function to enable
(defun listen-subsonic--search (query)
  "Search server for QUERY at \"search3\" endpoint.
Returns a list of tagged items. Each item is an alist with an added keyword `subsonic-type'."
  (let* ((max-results (number-to-string (/ listen-subsonic-search-max-results 3)))
         (params `(("query" . ,query)
                   ("artistCount" . ,max-results)
                   ("albumCount" . ,max-results)
                   ("songCount" . ,max-results)))
         (response (listen-subsonic--api-call "search3" params))
         (result (alist-get 'searchResult3 response))
         (artists (alist-get 'artist result))
         (albums (alist-get 'album result))
         (tracks (alist-get 'song result)))
    (nconc
     (mapcar (lambda (item) (cons '(subsonic-type . "Artist") item)) artists)
     (mapcar (lambda (item) (cons '(subsonic-type . "Album") item)) albums)
     (mapcar (lambda (item) (cons '(subsonic-type . "Track") item)) tracks))))

(defun listen-subsonic--search-suffix (item)
  "Return a suffix string for ITEM type."
  (pcase (alist-get 'subsonic-type item)
    ("Artist" "")
    ("Album" (concat (alist-get 'artist item)
                     (when-let* ((year (alist-get 'year item)))
                       (format " (%s)" year))))
    ("Track" (concat (alist-get 'artist item)
                     " - "
                     (alist-get 'album item)
                     (format " (%s)" (listen-format-seconds (or (alist-get 'duration item) 0)))))))

(defun listen-subsonic-search (query)
  "Search the server for QUERY, and display artists, albums and tracks.

- Selecting a track adds it to the queue.
- Selecting an artist or album opens the `listen-subsonic-find' browsing functionality."
  (interactive (list (read-string "Search: ")))
  (let* ((items (listen-subsonic--search query))
         (items-map (make-hash-table :test 'equal))
         (queue (listen-queue-complete :allow-new-p t)))

    (unless items
      (user-error "No search results for '%s'" query))

    ;; hashmap for completions
    (dolist (item items)
      (let* ((type (alist-get 'subsonic-type item))
             (name (if (string= type "Track")
                       (format "%s - %s" (alist-get 'artist item) (alist-get 'title item))
                     (alist-get 'name item)))
             (unique-name name)
             (count 1))
        ;; duplicates
        (while (gethash unique-name items-map)
          (cl-incf count)
          (setq unique-name (format "%s (%d)" name count)))
        (puthash unique-name item items-map)))

    (let* ((suffix-fn (lambda (cand)
                        (listen-subsonic--search-suffix (gethash cand items-map))))
           (group-fn (lambda (cand transform)
                       (if transform
                           cand
                         (alist-get 'subsonic-type (gethash cand items-map)))))
           (completion-extra-properties
            `(:affixation-function ,(listen-subsonic--affixation items-map suffix-fn 'listen-album)
              :group-function ,group-fn))
           (selected-name (completing-read "Select: " items-map nil t))
           (selected-item (gethash selected-name items-map))
           (type (alist-get 'subsonic-type selected-item))
           (auth (listen-subsonic--get-auth-params)))

      (pcase type
        ("Track"
         ;; add a track to the queue
         (let ((track (listen-subsonic--json-to-listen selected-item auth)))
           (listen-queue-add-tracks (list track) queue)
           (message "Added '%s' to the queue." (listen-track-title track))))
        (_ ; artist or album
         (let ((id (alist-get 'id selected-item))
               (name (alist-get 'name selected-item))
               (next (if (string= type "Artist") :artist :album)))
           ;; hand over to --find-step
           (when-let* ((result (listen-subsonic--find-step next id name)))
             (let ((tracks (funcall (nth 0 result))))
               (listen-queue-add-tracks tracks queue)
               (message "Added %d tracks from '%s'." (length tracks) name)))))))))

(defun listen-subsonic-queue-search-tracks (query queue)
  "Prompt for a search QUERY, and add its results to the current QUEUE."
  (interactive
   (list (read-string "Search: ")
         (listen-queue-complete :allow-new-p t)))
  (let* ((tracks (listen-subsonic-search-tracks query))
         (track (listen-subsonic--read-track tracks "Select track: ")))
    (progn
      (listen-queue-add-tracks (list track) queue)
      (message "Added '%s' to queue." (listen-track-title track)))
    (message "No tracks selected or found.")))

(defun listen-library-from-subsonic (&optional source)
  "Show a `listen-library' buffer with content from SOURCE.

SOURCE may be one of:
- \"Find\": Allows the user to browse a directory tree.
- \"Starred Tracks\": Library from starred tracks.
- \"Playlist\": Library from a playlist.
- \"Search\": Library from a search query."
  (interactive
   (list (completing-read "Source: "
                          '("Find"
                            "Starred Tracks"
                            "Playlist"
                            "Search")
                          nil t)))
  (let ((tracks-fn
         (pcase source
           ("Starred Tracks"
            (lambda () (listen-subsonic-get-starred-tracks)))
           ("Find"
            (lambda () (listen-subsonic-find)))
           ("Playlist"
            (lambda ()
              (listen-subsonic--get-playlist-tracks (listen-subsonic--read-playlist))))
           ("Search"
            (lambda ()
              (let ((query (read-string "Search: ")))
                (listen-subsonic-search-tracks query)))))))
    (listen-library tracks-fn
                    :name (format "Subsonic: %s" source))))

;; TODO: Make this send a clear cache request to server too?
(defun listen-subsonic-clear-cache ()
  "Delete the Subsonic cache directory and its contents."
  (interactive)
  (when (file-exists-p listen-subsonic-cache-dir)
    (delete-directory listen-subsonic-cache-dir t))
  (message "Cleared Subsonic cache."))

;; Completing read browser
;; TODO: unify the logic used by the minibuffer browser and the buffer browser
(defun listen-subsonic-find ()
  "Browse the Subsonic library hierarchy using `completing-read'.

Library hierarchy: Artist -> Album -> Song.
Select the \"[All]\" option to select all tracks under the current level.
Select the \"..\" option to move up/back in the hierarchy."
  (interactive)
  (let ((result (listen-subsonic--find-step :artists nil "Library")))
    (when result
      (if (called-interactively-p 'interactive)
          (listen-library (nth 0 result) :name (nth 1 result))
        (funcall (nth 0 result))))))

;; TODO: propertize everything properly
(defun listen-subsonic--find-step (level id name &optional history)
  "Recursive browser navigation function for `listen-subsonic-find'.
Returns a list (function name) for the selected action, or nil to go up/back.

LEVEL, ID, and NAME define the current location.
HISTORY is a stack containint the user's navigation history."
  (let* ((items (listen-subsonic--get-nodes level id))
         (node-map (make-hash-table :test 'equal))
         (next (listen-subsonic--browser-next-level level))
         (prompt (if (eq level :artists)
                     "Library: "
                   (let ((path (mapcar (lambda (h) (nth 2 h)) history)))
                     (format "%s / %s: " (string-join (reverse path) " / ") name)))))

    ;; When theres history, add an up option
    (when history
      (puthash (propertize ".." 'face 'shadow) :up node-map))

    ;; Prepare candidates
    (dolist (item items)
      (let* ((node-name (alist-get 'name item))
             (disp-name node-name)
             (count 1))
        (while (gethash disp-name node-map)
          (cl-incf count)
          (setq disp-name (format "%s (%d)" node-name count)))
        (puthash disp-name item node-map)))

    ;; ensure ".." and "[All]" are at the top
    ;; subsonic return is already sorted
    (let* ((completion-extra-properties
            `(:affixation-function ,(listen-subsonic--affixation
                                     node-map
                                     #'listen-subsonic--suffix-node
                                     'completions-annotations)
              :display-sort-function identity
              :cycle-sort-functions identity))
           (sel-name (completing-read prompt node-map nil t))
           (selection (gethash sel-name node-map)))

      ;; Handle user selection
      (cond
       ;; ".."
       ((eq selection :up)
        (apply #'listen-subsonic--find-step (car history))) ; Latest history
       ;; "[All]"
       ((eq selection :this)
        (list (lambda ()
                (listen-subsonic--get-all-tracks id level))
              (format "Subsonic: %s" name)))
       ;; Folder/artist/album
       ((and (alist-get 'isDir selection))
        (listen-subsonic--find-step next
                                    (alist-get 'id selection)
                                    sel-name
                                    (cons (list level id name history) history))) ; Add history
       ;; Song
       (t
        (list (lambda () (list (listen-subsonic--json-to-listen selection)))
              (format "Subsonic: %s" sel-name)))))))

;; dired-like browser UI
(defun listen-subsonic--dired-next-line (&optional n)
  "Move N lines down in the browser buffer.

Ensures the point is automatically placed on a text-button.
If N is negative, the point will move up instead.
If N is nil, the point will move down one line."
  (interactive)
  (line-move (or n 1) t)
  (beginning-of-line)
  (when (re-search-forward "[📁🎵] " (line-end-position) t)
    (goto-char (match-beginning 0))))

(defun listen-subsonic--dired-prev-line (&optional n)
  "Move N lines up in the browser buffer.

Ensures the point is automatically placed on a text-button.
If N is negative, the point will move down instead.
If N is nil, the point will move up one line."
  (interactive)
  (listen-subsonic--dired-next-line (- 0 (or n 1))))

(defun listen-subsonic--dired-star ()
  "Star/unstar the track, album or artist at point."
  (interactive)
  (let* ((pt (point))
         (item (get-text-property pt 'subsonic-item))
         ;; save buffer context for the callback
         (buf (current-buffer)))
    (unless item
      (user-error "No item on this line"))

    (let* ((id (alist-get 'id item))
           (star-p (not (alist-get 'starred item))))
      (listen-subsonic--star-item id star-p
                                  (lambda (_)
                                    (with-current-buffer buf
                                      (revert-buffer)
                                      (message "%s" (if star-p "Starred" "Unstarred"))))))))

(defvar listen-subsonic-dired-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "^") #'listen-subsonic--dired-up)
    (define-key map (kbd "g") #'revert-buffer)
    (define-key map (kbd "A") #'listen-subsonic--dired-add-all)
    (define-key map (kbd "n") #'listen-subsonic--dired-next-line)
    (define-key map (kbd "p") #'listen-subsonic--dired-prev-line)
    (define-key map (kbd "S") #'listen-subsonic--dired-star)
    (define-key map [remap next-line] #'listen-subsonic--dired-next-line)
    (define-key map [remap previous-line] #'listen-subsonic--dired-prev-line)
    map)
  "Keymap for `listen-subsonic-dired-mode'.")

(define-derived-mode listen-subsonic-dired-mode special-mode "Subsonic-Dired"
  "Major mode for browsing Subsonic libraries with a `dired'-like interface."
  :interactive nil
  :keymap listen-subsonic-dired-mode-map
  (setq-local revert-buffer-function #'listen-subsonic--dired-revert
              listen-subsonic--dired-history nil
              listen-subsonic--dired-current-id nil
              listen-subsonic--dired-current-name nil
              listen-subsonic--dired-current-level nil))

;; TODO: Allow user to star items with a keybind from this menu
(defun listen-subsonic-dired ()
  "Create or switch to the Listen Subsonic Dired buffer.

Interface opens at the :root level, showing the user's available folders."
  (interactive)
  (let ((buf (get-buffer-create "*Listen Subsonic Dired*")))
    (with-current-buffer buf
      (listen-subsonic-dired-mode)
      (listen-subsonic--dired-render nil "Library" :artists))
    (switch-to-buffer buf)))

(defun listen-subsonic--process-art-queue ()
  "Asynchronously process background art queue.

Asynchronous calls to the api use this function as the callback, this function gets called
recursively until the art queue is empty."
  (while (and listen-subsonic--art-queue
              (< listen-subsonic--art-active listen-subsonic--art-max))
    (cl-incf listen-subsonic--art-active)
    (pcase-let ((`(,url ,file ,buf ,pos) (pop listen-subsonic--art-queue)))
      (plz 'get url
        :as `(file ,file)
        :then (lambda (_)
                (cl-decf listen-subsonic--art-active)
                (listen-subsonic--display-art file buf pos)
                (listen-subsonic--process-art-queue))
        :else (lambda (_)
                (cl-decf listen-subsonic--art-active)
                (listen-subsonic--process-art-queue))))))

(defun listen-subsonic--dired-fetch-art (id buf pos)
  "Queue a download for artwork with ID to be displayed at POS in BUF.

If artwork exists in `listen-subsonic-cache-dir', that will be used. Otherwise, art will be
downloaded.
Art is asynchronously displayed in the Listen Subsonic Dired buffer as it is downloaded."
  (unless (file-exists-p listen-subsonic-cache-dir)
    (make-directory listen-subsonic-cache-dir))
  (let ((file (expand-file-name (format "%s.jpg" id) listen-subsonic-cache-dir))
        (url (listen-subsonic--build-url "getCoverArt"
                                         (append (listen-subsonic--get-auth-params)
                                                 `(("id" . ,id) ("size" . "64"))))))
    (if (file-exists-p file)
        ;; cached
        (listen-subsonic--display-art file buf pos)
      ;; download
      (push (list url file buf pos) listen-subsonic--art-queue)
      (listen-subsonic--process-art-queue))))

(defun listen-subsonic--display-art (file buf pos)
  "Display FILE's image in BUF at POS.

Used as the callback function for asynchronous art downloads in
`listen-subsonic--process-art-queue'."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (with-silent-modifications
        (let ((image (create-image file nil nil
                                   :ascent 'center
                                   :height 64)))
          (put-text-property pos (1+ pos) 'display image))))))

;; Render the "dired" buffer
(defun listen-subsonic--dired-insert-item (item next)
  "Insert a formatted line for ITEM into the current buffer.
Returns a list (art-id buffer pos) for asynchronous artwork downloads.

NEXT determines the level the ITEM will link to."
  (let* ((dirp (alist-get 'isDir item))
         (name (alist-get 'name item))
         (prefix (listen-subsonic--dired-get-prefix item))
         (pt (point))
         (face (if dirp
                   (pcase listen-subsonic--dired-current-level
                     (:artists 'listen-artist)
                     (:artist 'listen-album)
                     (_ 'listen-album))
                 'listen-title)))
    (insert (propertize prefix 'face 'shadow))
    (insert-text-button
     (concat (if dirp "📁 " "🎵 ") name)
     'action #'listen-subsonic--dired-button
     'follow-link t
     'subsonic-item item
     'subsonic-next (if dirp next nil)
     'face face)
    (insert "\n")
    ;; returns id and pos for art
    (list (or (alist-get 'coverArt item) (alist-get 'id item))
          (current-buffer)
          (+ pt (length prefix)))))

(defun listen-subsonic--dired-render (id name level)
  "Render the Dired-like browser buffer for hierarchy LEVEL and ID with NAME.

Inserts a header, navigation buttons and the list of items."
  (setq-local listen-subsonic--dired-current-id id
              listen-subsonic--dired-current-name name
              listen-subsonic--dired-current-level level)
  (let* ((inhibit-read-only t)
         (items (listen-subsonic--get-nodes level id))
         (next (listen-subsonic--browser-next-level level)))

    ;; prepare buffer
    (erase-buffer)
    ;; "path" of current level
    (let* ((parents (mapcar (lambda (h) (nth 1 h)) listen-subsonic--dired-history))
           (path (reverse (cons name parents))))
      (insert (propertize (string-join path " / ") 'face 'dired-header) "\n"))
    ;; "." to revert buffer/refresh
    ;; ".." to go up (same as "^" bind)
    (insert-text-button "."
                        'action (lambda (_) (revert-buffer))
                        'follow-link t
                        'face 'dired-directory)
    (insert "\n")
    (when listen-subsonic--dired-history
      (insert-text-button ".."
                          'action (lambda (_) (listen-subsonic--dired-up))
                          'follow-link t
                          'face 'dired-directory)
      (insert "\n"))
    (let ((next (listen-subsonic--browser-next-level level)))
      (dolist (item (listen-subsonic--get-nodes level id))
        (pcase-let ((`(,art-id ,buf ,pos)
                     (listen-subsonic--dired-insert-item item next)))
          (when (and art-id (not (eq level :artists)))
            (listen-subsonic--dired-fetch-art art-id buf pos)))))
    (beginning-of-buffer)
    (listen-subsonic--dired-next-line)))

;; browser functions

(defun listen-subsonic--dired-add-all ()
  "Fetch all tracks in/under the current view and add them to the current queue."
  (interactive)
  (let ((tracks (pcase listen-subsonic--dired-current-level
                  (:artist
                   (listen-subsonic--get-all-tracks listen-subsonic--dired-current-id :artist))
                  (:album
                   (listen-subsonic--get-all-tracks listen-subsonic--dired-current-id :album))
                  (_
                   (user-error "Cannot add all tracks under the current view")))))
    (when tracks
      (listen-queue-add-tracks tracks (listen-queue-complete))
      (message "Added %d tracks to the queue." (length tracks)))
    (message "No tracks found.")))

(defun listen-subsonic--dired-button (&optional button)
  "Activate the text BUTTON at point.

If button at point is a directory, render the next level.
If button at point is a track, add it to the current queue."
  (interactive)
  (let* ((pt (if button (button-start button) (point)))
         (item (get-text-property pt 'subsonic-item))
         (next (get-text-property pt 'subsonic-next)))
    (unless item (user-error "No item on this line"))

    ;; open directory
    (if (alist-get 'isDir item)
        (progn
          ;; add current state to history
          (push (list listen-subsonic--dired-current-id
                      listen-subsonic--dired-current-name
                      listen-subsonic--dired-current-level)
                listen-subsonic--dired-history)
          ;; display next level
          (listen-subsonic--dired-render (alist-get 'id item)
                                         (or (alist-get 'title item) (alist-get 'name item))
                                         next))
      ;; song
      (listen-queue-add-tracks (list (listen-subsonic--json-to-listen item))
                               (listen-queue-complete))
      (message "Added '%s' to queue." (alist-get 'title item)))))

(defun listen-subsonic--dired-up ()
  "Navigate to the parent directory in the browser history.

Pops the previous state from `listen-subsonic--dired-history'."
  (interactive)
  (if-let ((prev (pop listen-subsonic--dired-history)))
      (listen-subsonic--dired-render (nth 0 prev) (nth 1 prev) (nth 2 prev))
    (message "This is the highest level.")))

(defun listen-subsonic--dired-revert (_ignore-auto _noconfirm)
  "Reload the current browser view.

Re fetches data for the current ID and level from the API."
  (let ((pt (point)))
    (listen-subsonic--dired-render listen-subsonic--dired-current-id
                                   listen-subsonic--dired-current-name
                                   listen-subsonic--dired-current-level)
    (goto-char pt)
    ;; Ensure we are snapped to the button
    (listen-subsonic--dired-next-line 0)))

(provide 'listen-subsonic)
;;; listen-subsonic.el ends here
