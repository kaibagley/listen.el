;;; listen-subsonic.el --- Subsonic server support for listen.el         -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Free Software Foundation, Inc.

;; Author: Kai Bagley <kaibagley@proton.mail>

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

;;

;;; Code:

;;;; Requirements

;; TODO: Some kind of indicator to show if track is starred or not
(require 'plz)          ; HTTP requests
(require 'auth-source)  ; authinfo
(require 'listen-queue) ; Add tracks to queue

(require 'map)          ; for map-let and map-elt
(require 'url-util)     ; for url-build-query-string

;; Declares

(declare-function listen-library "listen-library")

;;;; Variables

(defvar listen-subsonic--auth-cache nil
  "Cache for auth parameters to avoid recomputation.")

;;;; Customisation

(defgroup listen-subsonic nil
  "`listen' options for Subsonic."
  :group 'listen)

(defcustom listen-subsonic-url nil
  "The base URL of your Subsonic-compatible server.
e.g., \"music.example.com\""
  :type 'string
  :group 'listen-subsonic)

(defcustom listen-subsonic-search-max-results "50"
  "Maximum results to return in search queries.
Must be a string."
  :type 'string
  :group 'listen-subsonic)

(defcustom listen-subsonic-user-agent "listen.el"
  "User-agent used in API requests."
  :type 'string
  :group 'listen-subsonic)

(defface listen-starred
  '((t :inherit font-lock-warning-face))
  "Face for starred Subsonic tracks."
  :group 'listen-subsonic)

;;;; Auth helpers

(defun listen-subsonic--get-credentials ()
  "Fetch user credentials securely from `auth-source`."
  (let ((auth (auth-source-search :host listen-subsonic-url)))
    (when auth
      (car auth))))

(defun listen-subsonic--get-auth-params ()
  "Return auth info alist for API calls."
  (or listen-subsonic--auth-cache
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
          ("f" . "json")))))

;; TODO: Maybe allow insecure http later?
(defun listen-subsonic--build-url (endpoint params)
  "Build a URL from ENDPOINT and PARAMS, to be used as an API call to
Subsonic."
  (let* ((param-list (mapcar (lambda (p)
                               (list (car p) (cdr p)))
                             params))
         (param-str (url-build-query-string param-list nil t)))
    (format "https://%s/rest/%s.view?%s"
            listen-subsonic-url endpoint param-str)))

;;;; API Helpers

(defun listen-subsonic--get-stream-url (id)
  "Return a URL for MPV to stream from directly.
Includes token, salt, and username retrieved from `auth-source' as
parameters."
  (listen-subsonic--build-url
   "stream"
   (append (listen-subsonic--get-auth-params) `(("id" . ,id)))))

(defun listen-subsonic--json-to-listen (s)
  "Convert JSON alist S into a `listen-track' structure."
  (map-let (('id id) ('userRating rating) artist title album track genre duration year starred) s
    (make-listen-track
     :filename (listen-subsonic--get-stream-url id) ; silly mpv
     :artist artist
     :title title
     :album album
     :number (number-to-string (or track 0))
     :genre genre
     :duration (or duration 0)
     :date year
     ;; Rating is a string, "0.0" - "1.0". Subsonic returns 0-5 or nil
     :rating (when rating (format "%f" (/ rating 5.0)))
     :metadata s
     :etc `((source . "subsonic")
            (id . ,id)
            (starred . ,(if starred t nil))))))

(defun listen-subsonic--process-api-response ()
  "Parse JSON response from a Subsonic API request.
Returns the response's data, or signals an error.
Should be called from a buffer containing an API response."
  (goto-char (point-min))
  (when (zerop (buffer-size))
    (error "Subsonic API response is empty"))
  (let* ((json-data (json-parse-buffer :object-type 'alist
                                       :null-object nil
                                       :false-object nil))
         (response (alist-get 'subsonic-response json-data)))
    (unless (string-equal "ok" (alist-get 'status response))
      (error "Subsonic API response returned error: %s"
             (alist-get 'message (alist-get 'error response))))
    response))

(defun listen-subsonic--api-call (endpoint &optional params callback)
  "Make a call to the Subsonic API.
ENDPOINT is the API method defined by the Subsonic or OpenSubsonic API specifications.
PARAMS is an alist of additional parameters.
If CALLBACK is nil, run synchronously and return the parsed JSON.
If CALLBACK is non-nil, run asynchronously and call CALLBACK with the data."
  (unless listen-subsonic-url
    (user-error "Please set `listen-subsonic-url'."))
  (let* ((api-params (append (listen-subsonic--get-auth-params) params))
         (api-url (listen-subsonic--build-url endpoint api-params))
         (api-headers '(("Accept-Encoding" . "gzip"))))
    (plz 'get api-url
      :headers api-headers
      :as #'listen-subsonic--process-api-response
      :then (or callback 'sync))))

(defun listen-subsonic--ensure-list (item)
  "Ensure ITEM is a list."
  (cond
   ((vectorp item) (append item nil))
   ((null item) nil)
   ((and (listp item) (consp (car item))) (list item))
   (t (list item))))

;;;; Read requests

(defun listen-subsonic--get-tracks (endpoint rootkey itemkey &optional params)
  "Return tracks from Subsonic REST ENDPOINT.
Returned alist is the contents of ROOTKEY, then ITEMKEY of the API
response. PARAMS are optional API parameters.

The parsed API response consists of an alist which is mostly metadata
and a data structure labelled ROOTKEY. This data is another alist with
metadata about the request data, and the interesting part of the
request labelled ITEMKEY."
  (let* ((response (listen-subsonic--api-call endpoint params))
         (data (alist-get rootkey response))
         (tracks (alist-get itemkey data))
         ;; Let bind cached auth params
         (listen-subsonic--auth-cache (listen-subsonic--get-auth-params)))
    (mapcar #'listen-subsonic--json-to-listen tracks)))

(defun listen-subsonic--get-browse (endpoint rootkey itemkey)
  "Return an alist from Subsonic ENDPOINT as (name . id).
The \"browse\" API endpoints have similarly structured responses.
Returned alist is the contents of ROOTKEY, then ITEMKEY of the
API response.

The parsed API response consists of an alist which is mostly metadata
and a data structure labelled ROOTKEY. This data is another alist with
metadata about the request data, and the interesting part of the
request labelled ITEMKEY."
  (let* ((response (listen-subsonic--api-call endpoint))
         (data (alist-get rootkey response))
         (items (alist-get itemkey data)))
    (mapcar (lambda (item)
              (cons (alist-get 'name item)
                    (format "%s" (alist-get 'id item)))) ; This must be a string
            items)))

;; TODO: This blocks emacs while waiting for response
;; Look into consult's async features at some
;; stage? Maybe not necessary but will allow searching way more than 50
(defun listen-subsonic-search-tracks (query)
  "Return a list of `listen-track' objects.
Uses the Subsonic API's \"search3\" endpoint with QUERY as the search query.
The maximum returned tracks is 50."
  (listen-subsonic--get-tracks "search3" 'searchResult3 'song
                               `(("query" . ,query) ("songCount" . ,listen-subsonic-search-max-results))))

(defun listen-subsonic-get-starred-tracks ()
  "Fetch all starred songs from Subsonic server."
  (listen-subsonic--get-tracks "getStarred" 'starred 'song))

(defun listen-subsonic--get-playlists ()
  "Return a list of all playlists accessible to the user."
  (listen-subsonic--get-browse "getPlaylists" 'playlists 'playlist))

(defun listen-subsonic--get-playlist-tracks (playlist)
  "Return all tracks in PLAYLIST."
  (listen-subsonic--get-tracks "getPlaylist"
                               'playlist 'entry
                               `(("id" . ,playlist))))

;;;; Write requests

(defun listen-subsonic-star-track (track star-p)
  "Send a request to the \"star\" or \"unstar\" Subsonic endpoints.
Star (when STAR-P is non-nil) or unstar TRACK.
When called interactively, the star-state of the song will be toggled."
  (interactive
   (let ((track (listen-queue-complete-track (listen-queue-complete))))
     (list track (not (alist-get 'starred (listen-track-etc track))))))
  (when-let* ((id (alist-get 'id (listen-track-etc track))))
    (listen-subsonic--api-call (if star-p "star" "unstar")
                               `(("id" . ,id))
                               (lambda (_)
                                 (setf (alist-get 'starred (listen-track-etc track)) star-p)
                                 (message "%s '%s'" (if star-p "Starred" "Unstarred")
                                          (listen-track-title track))))))

(defun listen-subsonic--scrobble (player submission-p)
  "Scrobble the current track playing in PLAYER's queue to the Subsonic API.
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
  "Notifies the Subsonic server that we have started playing a track.
Should be added to `listen-track-start-functions'."
  (listen-subsonic--scrobble player nil))

(defun listen-subsonic-scrobble-end (player)
  "Notifies the Subsonic server that we have finished a track.
Should be added to `listen-track-end-functions'."
  (listen-subsonic--scrobble player t))

;;;; Interactive functions

(defun listen-subsonic-ping-server ()
  "Ping the server to check connectivity and authentication."
  (interactive)
  (if (listen-subsonic--api-call "ping")
      (message "Successfully pinged Subsonic server!")
    (message "Failed to ping server.")))

(defun listen-subsonic-queue-random (n queue)
  "Fetch and add to QUEUE a list of N random songs."
  (interactive
   (list
    (read-number "Number of songs: " 10)
    (listen-queue-complete :allow-new-p t)))
  (let* ((tracks (listen-subsonic--get-tracks
                  "getRandomSongs"
                  'randomSongs 'song
                  `(("size" . ,(number-to-string n))))))
    (if tracks
        (progn
          (listen-queue-add-tracks tracks queue)
          (message "Added %d random tracks to queue '%s'."
                   (length tracks) (listen-queue-name queue))
          (listen-queue queue))
      (message "No tracks returned from server."))))

;; TODO: Deduplicate logic between this and the library code below
(defun listen-subsonic-queue-playlist (queue)
  "Add all tracks from a user's playlist to the QUEUE."
  (interactive (list (listen-queue-complete :allow-new-p t)))
  (let* ((playlists (listen-subsonic--get-playlists))
         (name (completing-read "Playlist: " playlists nil t))
         (id (alist-get name playlists nil nil #'equal))
         (tracks (listen-subsonic--get-playlist-tracks id)))
    (if tracks
        (progn
          (listen-queue-add-tracks tracks queue)
          (message "Added %d tracks to queue '%s'."
                   (length tracks) (listen-queue-name queue)))
      (message "No tracks found."))))

(defun listen-subsonic-queue-starred-tracks (queue)
  "Add all starred songs from Subsonic server to QUEUE."
  (interactive (list (listen-queue-complete :allow-new-p t)))
  (let ((tracks (listen-subsonic-get-starred-tracks)))
    (if tracks
        (progn
          (listen-queue-add-tracks tracks queue)
          (message "Added %d tracks to queue '%s'."
                   (length tracks) (listen-queue-name queue))
          (listen-queue queue))
      (message "No starred songs found."))))

;; TODO: C-u adds to start of queue/next?
;; TODO; Use annotate-function to make this (and other functions) look better
(defun listen-subsonic-queue-search-tracks (query queue)
  "Search Subsonic server for QUERY and add results to the current queue."
  (interactive
   (list
    (read-string "Search Subsonic: ")
    (listen-queue-complete :allow-new-p t)))
  (let* ((tracks (listen-subsonic-search-tracks query))
         (candidates (mapcar (lambda (track)
                               (cons (format "%s - %s (%s)"
                                             (listen-track-artist track)
                                             (listen-track-title track)
                                             (listen-track-album track))
                                     track))
                             tracks))
         (selected-names (if tracks
                             (completing-read-multiple "Select tracks (CRM): "
                                                       candidates
                                                       nil t)
                           nil))
         (selected-tracks (mapcar (lambda (name)
                                    (alist-get name candidates nil nil #'equal))
                                  selected-names)))
    (if selected-tracks
        (progn
          (listen-queue-add-tracks selected-tracks queue)
          (message "Added %d tracks from Subsonic to queue '%s'."
                   (length selected-tracks)
                   (listen-queue-name queue))
          (listen-queue queue))
      (message "No tracks found or added to '%s'" query))))

;; TODO: merge this with get-folder-tracks to simplify browse code
(defun listen-subsonic--get-all-tracks (id)
  "Fetch all tracks under directory ID recursively."
  (let* ((data (listen-subsonic--api-call "getMusicDirectory" `(("id" . ,id))))
         (parent (alist-get 'directory data))
         (children (alist-get 'child parent))
         ;; Let bind cached variables
         (listen-subsonic--auth-cache (listen-subsonic--get-auth-params)))
    (mapcan (lambda (c)
              (if (alist-get 'isDir c)
                  (listen-subsonic--get-all-tracks (alist-get 'id c))
                (list (listen-subsonic--json-to-listen c))))
            children)))

(defun listen-subsonic--get-folder-tracks (id)
  "Return all tracks in music folder ID."
  (listen-subsonic--get-tracks "search3" 'searchResult3 'song
                               `(("musicFolderId" . ,id)
                                 ("query" . "")
                                 ("songCount" . "100000"))))

;; Directory browsing functions
(defun listen-subsonic--get-nodes (level id)
  "Return a list of items from browsing LEVEL using ID."
  (let ((items
         (pcase level
           (:root
            (let* ((data (listen-subsonic--api-call "getMusicFolders"))
                   (items (listen-subsonic--ensure-list
                           (map-nested-elt data '(musicFolders musicFolder)))))
              (mapcar (lambda (item) (cons '(isDir . t) item)) items)))
           (:indexes
            (let ((data (listen-subsonic--api-call "getIndexes" `(("musicFolderId" . ,id)))))
              (listen-subsonic--flatten-indexes data)))
           (:directory
            (let ((data (listen-subsonic--api-call "getMusicDirectory" `(("id" . ,id)))))
              (listen-subsonic--ensure-list (map-nested-elt data '(directory child))))))))
    ;; ensure every item has a 'name
    (mapcar (lambda (item)
              (if (alist-get 'title item)
                  (cons (cons 'name (alist-get 'title item)) item)
                item))
            items)))

;; completing-read browser

(defun listen-subsonic--browse-step (level id name &optional history)
  (let* ((items (listen-subsonic--get-nodes level id))
         (candidates (mapcar (lambda (item) (cons (alist-get 'name item) item)) items))
         (next (listen-subsonic--browse-next-level level))
         (prompt (if (eq level :root) "Library: " (format "%s: " name))))

    (let* ((choices (append
                     ;; When theres history, add an up option
                     (when history
                       '((".." . :up)))
                     ;; Dont show "All" for root (too much)
                     (unless (eq level :root)
                       '(("[All]" . :this)))
                     candidates))
           (sel-name (completing-read prompt (mapcar #'car choices) nil t))
           (selection (cdr (assoc sel-name choices))))
      (cond
       ;; User selected ".."
       ((eq selection :up)
        (apply #'listen-subsonic--browse-step (car history))) ; Latest history
       ;; User selected All
       ((eq selection :this)
        (list (lambda ()
                (pcase level
                  (:indexes
                   (listen-subsonic--get-folder-tracks id))
                  (_
                   (listen-subsonic--get-all-tracks id))))
              (format "Subsonic: %s" name)))
       ;; Descending a level
       ((and (alist-get 'isDir selection))
        (listen-subsonic--browse-step next
                                      (alist-get 'id selection)
                                      sel-name
                                      (cons (list level id name history) history))) ; Add history
       ;; Song/bottom level
       (t
        (list (lambda () (list (listen-subsonic--json-to-listen selection)))
              (format "Subsonic: %s" sel-name)))))))

(defun listen-subsonic-browse-library ()
  "Browse the Subsonic library hierarchy.
Library hierarchy: Folder -> Artist -> Album -> Song.
Select the \"[All]\" option to select all tracks under the current level."
  (interactive)
  (let ((result (listen-subsonic--browse-step :root nil "Root")))
    (when result
      (if (called-interactively-p 'interactive)
          (listen-library (nth 0 result) :name (nth 1 result))
        (funcall (nth 0 result))))))

(defun listen-library-from-subsonic (source)
  "Show a library view for subsonic."
  (interactive
   (list (completing-read "Source: "
                          '("Browse"
                            "Starred Tracks"
                            "Playlist"
                            "Search")
                          nil t)))
  (let ((tracks-fn
         (pcase source
           ("Starred Tracks"
            (lambda () (listen-subsonic-get-starred-tracks)))
           ("Browse"
            (lambda () (listen-subsonic-browse-library)))
           ("Playlist"
            (lambda ()
              (let* ((playlists (listen-subsonic--get-playlists))
                     (name (completing-read "Playlist: " playlists nil t))
                     (id (alist-get name playlists nil nil #'equal)))
                (listen-subsonic--get-playlist-tracks id))))
           ("Search"
            (lambda ()
              (let ((query (read-string "Search: ")))
                (listen-subsonic-search-tracks query)))))))
    (listen-library tracks-fn
                    :name (format "Subsonic: %s" source))))

(provide 'listen-subsonic)
;;; listen-subsonic.el ends here
