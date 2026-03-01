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

;;

;;; Code:

;;;; Requirements

;; TODO: Some kind of indicator to show if track is starred or not
;; TODO: Send bookmark request to server periodically

(require 'infrasonic)   ; For Subsonic backend
(require 'listen-queue) ; Add tracks to queue
(require 'svg-lib)      ; For starred icon

(require 'subr-x)       ; string-empty-p
(require 'map)          ; map-let/elt
(require 'cl-lib)       ; cl-incf
(require 'transient)    ; Transient menus

;; Declares

(declare-function listen-library "listen-library")

;;;; Customisation

;;;###autoload
(defun listen-subsonic--build-client ()
  "Build or rebuild our `listen-subsonic--client' from `listen' user options."
  (setq listen-subsonic--client
        (condition-case nil
            (infrasonic-make-client
             :url listen-subsonic-url
             :protocol listen-subsonic-protocol
             :user-agent "listen.el"
             :api-version listen-subsonic-api-version
             :queue-limit listen-subsonic-queue-limit
             :timeout listen-subsonic-timeout
             :art-size 128
             :search-max-results listen-subsonic-search-max-results)
          (infrasonic-error nil))))

;;;###autoload
(defun listen-subsonic--custom-set (symbol value)
  "Rebuild the `infrasonic' client with SYMBOL set to VALUE."
  (set-default symbol value)
  (listen-subsonic--build-client))

;;;###autoload
(defgroup listen-subsonic nil
  "`listen' options for Subsonic backend."
  :group 'listen)

;;;###autoload
(defcustom listen-subsonic-url nil
  "The fully-qualified domain name of your Subsonic-compatible server.
For example, \"music.example.com\" or \"192.168.0.0:4533\".
Don't include the procol/scheme or the resource path."
  :type 'string
  :group 'listen-subsonic
  :set #'listen-subsonic--custom-set)

;;;###autoload
(defcustom listen-subsonic-protocol "https"
  "Protocol to use for calls to Subsonic API.
Must be either \"http\" or \"https\" (default)."
  :type '(choice (const :tag "HTTPS" "https")
                 (const :tag "HTTP" "http"))
  :group 'listen-subsonic
  :set #'listen-subsonic--custom-set)

;;;###autoload
(defcustom listen-subsonic-api-version "1.16.1"
  "OpenSubsonic API version string to advertise (e.g. \"1.16.1\")."
  :type 'string
  :group 'listen-subsonic
  :set #'listen-subsonic--custom-set)

;;;###autoload
(defcustom listen-subsonic-timeout 300
  "Request timeout in seconds passed to `plz'."
  :type 'integer
  :group 'listen-subsonic
  :set #'listen-subsonic--custom-set)

;;;###autoload
(defcustom listen-subsonic-queue-limit 5
  "Max concurrent downloads for `infrasonic''s `plz' queue."
  :type 'integer
  :group 'listen-subsonic
  :set #'listen-subsonic--custom-set)

;;;###autoload
(defcustom listen-subsonic-search-max-results 200
  "Maximum number of results returned by search queries."
  :type 'integer
  :group 'listen-subsonic
  :set #'listen-subsonic--custom-set)

;; Users set infrasonic variables for URL, protocol, etc.

(defface listen-starred
  '((t :inherit font-lock-warning-face))
  "Face for starred Subsonic tracks."
  :group 'listen-subsonic)

(defvar listen-subsonic--client nil
  "Current `infrasonic' client.")

(defvar listen-subsonic-cache-dir (expand-file-name "listen.el" temporary-file-directory)
  "Directory to store cached files such as cover art.")

(defvar listen-subsonic--menu-max-width 50
  "Maximum width of strings returned by search function.")

(defvar listen-subsonic--image-cache (make-hash-table :test 'equal)
  "Cache of image descriptors keyed by (TRACK-ID . SIZE).")

;;;; General helpers

(defun listen-subsonic--client ()
  "Return the current `infrasonic' client, or build a new one and return that."
  (or listen-subsonic--client
      (progn
        (listen-subsonic--build-client)
        (or listen-subsonic--client
            (user-error "Please set `listen-subsonic-url'.")))))

(defun listen-subsonic--json-to-listen (json-data &optional client)
  "Convert an `infrasonic' JSON-DATA into a `listen-track'.
Returns a `listen-track' struct."
  (let ((client (or client (listen-subsonic--client))))
    (map-let
        (('id id) ('userRating rating) artist title album track genre duration year starred)
        json-data
      (make-listen-track
       :filename (infrasonic-get-stream-url client id) ; silly mpv
       :artist artist
       :title title
       :album album
       :number (when track (number-to-string track))
       :genre genre
       :duration (or duration 0)
       :date year
       ;; Rating is a string, "0.0" - "1.0". Subsonic returns 0-5 or nil
       :rating (when rating (format "%f" (/ rating 5.0)))
       :metadata json-data
       :etc `((source . "subsonic")
              (id . ,id)
              (starred . ,(when starred t)))))))

;;;; Read requests

(defun listen-subsonic-search-tracks (query)
  "Search the server for tracks matching QUERY.
Returns a list of `listen-track's."
  (mapcar #'listen-subsonic--json-to-listen
          (infrasonic-search-songs (listen-subsonic--client) query nil)))

(defun listen-subsonic-get-starred-tracks ()
  "Fetch all starred songs from the server.
Returns a list of `listen-track's."
  (mapcar #'listen-subsonic--json-to-listen
          (infrasonic-get-starred-songs (listen-subsonic--client))))

(defun listen-subsonic--get-playlist-tracks (id)
  "Fetch all tracks in playlist with ID.
Returns a list of `listen-track's."
  (mapcar #'listen-subsonic--json-to-listen
          (infrasonic-get-playlist-songs (listen-subsonic--client) id)))

(defun listen-subsonic--get-all-tracks (id level)
  "Fetch all tracks under item associated with ID.
Returns a list of `listen-track's.

LEVEL determines what level of the hierarchy we are on:
- :artist: fetches all albums, then all songs by that artist.
- :album: fetches all songs on the album."
  (mapcar #'listen-subsonic--json-to-listen
          (infrasonic-get-all-songs (listen-subsonic--client) id level)))

;;;; Write requests

(defun listen-subsonic-create-playlist (queue name)
  "Create a Subsonic playlist named NAME from tracks in QUEUE.
Returns the response data from a call to \"createPlaylist\".

Only tracks with the source \"subsonic\" will be included."
  (interactive
   (list (listen-queue-complete)
         (read-string "Playlist name: ")))
  (let ((ids (mapcan (lambda (track)
                       (let ((etc (listen-track-etc track)))
                         (when (equal (alist-get 'source etc) "subsonic")
                           (list (alist-get 'id etc)))))
                     (listen-queue-tracks queue))))
    (if ids
        (infrasonic-create-playlist (listen-subsonic--client) ids name)
      (user-error "No Subsonic tracks found"))))

(defun listen-subsonic--scrobble (player status &optional callback errback)
  "Scrobble the STATUS of the current track playing in PLAYER's queue to
the Subsonic API.
Returns the unparsed API response.

Only tracks with the source \"subsonic\" will be scrobbled.

STATUS may be either `:playing' or `:finished'.

CALLBACK and ERRBACK are optional parameters enabling asynchronous scrobbling."
  (when-let* ((queue (map-elt (listen-player-etc player) :queue))
              (track (listen-queue-current queue))
              (source (equal (alist-get 'source (listen-track-etc track)) "subsonic"))
              (id (alist-get 'id (listen-track-etc track))))
    (infrasonic-scrobble (listen-subsonic--client) id status callback errback)))

(defun listen-subsonic-scrobble-start (player)
  "Notifies the server that we have started playing a track in PLAYER.
Should be added to `listen-track-start-functions'."
  (listen-subsonic--scrobble player
                             :playing
                             #'ignore
                             (lambda (err)
                               (display-warning 'listen-subsonic
                                                (format "Scrobble error: %s" err)
                                                :warning))))

(defun listen-subsonic-scrobble-end (player)
  "Notifies the server that we have finished a track in PLAYER.
Should be added to `listen-track-end-functions'."
  (listen-subsonic--scrobble player
                             :finished
                             #'ignore
                             (lambda (err)
                               (display-warning 'listen-subsonic
                                                (format "Scrobble error: %s" err)
                                                :warning))))

;;;; Interactive functions

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
    (infrasonic-star (listen-subsonic--client)
                     id star-p
                     ;; update track in-memory
                     (lambda (_)
                       (setf (alist-get 'starred (listen-track-etc track)) star-p)
                       (message "%s '%s'" (if star-p "Starred" "Unstarred")
                                (listen-track-title track))))))

(defun listen-subsonic--completing-read (prompt entries &optional extra-metadata)
  "Read a candidate with PROMPT from ENTRIES.
Returns the chosen item.

ENTRIES is an alist of display strings, and its corresponding
value ((disp-str . item) ...).

EXTRA-METADATA is an alist of completion metadata pairs for
`completing-read', to be `cons'ed with
(category . listen-subsonic). For example:
'((affixation-function . <fn>)
  (group-function . <fn>)
  (display-sort-function . identity)
  (cycle-sort-function . identity))."
  (let* ((candidates (mapcar #'car entries))
         (default-metadata '((category . listen-subsonic)))
         (metadata (cons 'metadata (append default-metadata extra-metadata)))
         (table (completion-table-with-metadata candidates metadata))
         (selection (completing-read prompt table nil t)))
    (alist-get selection entries nil nil #'equal)))

(defun listen-subsonic--affixation (entries &optional suffix-fn suffix-face prefix-fn prefix-face)
  "Create an affixation function for `completing-read' using ENTRIES.
Returns a list of lists, where each element is (candidate prefix suffix)

ENTRIES is an alist of display strings and their corresponding item: ((disp-str . item) ...).
Where an item in the ENTRIES alist may be:
- the symbol :up or :this for special candidates such as \"..\" and \"[All]\",
- a Subsonic JSON alist for normal nodes.

SUFFIX-FN returns the suffix string from the object found in HASHTABLE. When nil, no suffix is
applied.

SUFFIX-FACE is applied to the suffix.

PREFIX-FN returns a prefix string from the object found in HASHTABLE. When nil,no prefix is applied.

PREFIX-FACE is applied to the prefix."
  (lambda (cands)
    (mapcar
     (lambda (cand)
       (let ((item (alist-get cand entries nil nil #'equal)))
         (if (memq item '(:up :this))
             (list cand "  " "")
           (let* ((disp (truncate-string-to-width (or cand "")
                                                  listen-subsonic--menu-max-width
                                                  0 ?\s t))
                  (disp-id (propertize cand 'display disp))
                  (len (string-width disp))
                  (pad (max 0 (- listen-subsonic--menu-max-width len)))
                  (padding (make-string pad ?\s))
                  (suf (if suffix-fn (funcall suffix-fn item) ""))
                  (suffix (if suffix-face (propertize suf 'face suffix-face) suf))
                  (pre (if prefix-fn (funcall prefix-fn item) ""))
                  (prefix (if prefix-face (propertize pre 'face prefix-face) pre)))
             (list disp-id prefix (concat padding suffix))))))
     cands)))

(defun listen-subsonic--comp-sorter (entries comp)
  "Convert a binary COMP function comparing ENTRIES to a sort function.
Returns a sort function for sorting `completing-read' candidates.

COMP is a binary (lambda (album-a album-b) ...) and is applied after
mapping candidate strings back to album objects via ENTRIES."
  (lambda (cands)
    (sort (copy-sequence cands)
          (lambda (sa sb)
            (funcall comp
                     (alist-get sa entries nil nil #'equal)
                     (alist-get sb entries nil nil #'equal))))))

(defun listen-subsonic--format-column (str width &optional face)
  "Format STR to fit WIDTH.
If shorter, pad with spaces. If longer, truncate with ellipsis.
Apply FACE if non-nil."
  (let ((s (truncate-string-to-width (or str "") width 0 ?\s t)))
    (if face (propertize s 'face face) s)))

(defun listen-subsonic--item-suffix (item)
  "Return a suffix string for ITEM type.

ITEM must include element with `car' \"subsonic-type\" for determining which suffix to use."
  (pcase (alist-get 'subsonic-type item)
    (:artist
     (format " %s albums" (or (alist-get 'albumCount item) 0)))
    (:album
     (concat " "
             (listen-subsonic--format-column (alist-get 'artist item)
                                             20 'listen-artist)
             " "
             (when-let* ((year (alist-get 'year item)))
               (format "%s" year))
             " "
             (propertize (when-let* ((pc (alist-get 'playCount item)))
                           (format "(%s plays)" pc))
                         'face 'shadow)))
    (:song
     (concat " "
             (listen-subsonic--format-column (alist-get 'artist item)
                                             20 'listen-artist)
             " "
             (listen-subsonic--format-column (alist-get 'album item)
                                             20 'listen-album)
             " "
             (listen-format-seconds (or (alist-get 'duration item) 0))))
    (_ "")))

(defun listen-subsonic--item-prefix (item)
  "Return a prefix string for ITEM type.

ITEM must include element with `car' \"starred\"."
  (format "%s "
          (if (alist-get 'starred item)
              (propertize " " 'display
                          (svg-lib-icon "star" 'listen-starred
                                        :stroke 0 :margin -2 :background nil))
            " ")))

(defun listen-subsonic--playlist-suffix (playlist)
  "Returns PLAYLIST's song count to be used as an `affixation-function' suffix."
  (concat (number-to-string (or (alist-get 'songCount playlist) 0)) " tracks"))

(defun listen-subsonic--read-playlist ()
  "Prompt user to select a Subsonic playlist using `completing-read'.
Returns the selected playlist's ID as a string."
  (let* ((playlists (infrasonic-get-playlists (listen-subsonic--client)))
         (name (completing-read "Playlist: " playlists nil t)))
    (alist-get name playlists nil nil #'equal)))

(defun listen-subsonic--read-album (albums &optional prompt sort-comp affix-fn)
  "Prompt user to select a Subsonic album using `completing-read'.
Returns the selected album's ID as a string.

PROMPT is an optional string for the prompt, defaunting to \"Album: \".

SORT-FN is an optional binary function (lambda (album1 album2) ...). It
is passed as completion metadata `display-sort-function' and
`cycle-sort-function' for sorting the `completing-read' interface.
Defaults to nil.

AFFIX-FN allows decorating entries in the `completing-read' interface.
Defaults to a star prefix, and album suffix."
  (let* ((prompt (or prompt "Album: "))
         (entries
          (mapcar (lambda (album)
                    (let* ((typed (cons (cons 'subsonic-type :album) album))
                           (disp (or (alist-get 'name typed) "[unknown album]")))
                      (cons disp typed)))
                  albums))
         (sort-fn (listen-subsonic--comp-sorter entries sort-comp))
         (affix-fn (or affix-fn
                       (listen-subsonic--affixation
                        entries
                        #'listen-subsonic--item-suffix nil
                        #'listen-subsonic--item-prefix nil)))
         (extra-metadata `((affixation-function . ,affix-fn)
                           (display-sort-function . ,sort-fn)
                           (cycle-sort-function . ,sort-fn))))
    (listen-subsonic--completing-read prompt entries extra-metadata)))

(defun listen-subsonic-get-random-tracks (n)
  "Fetch N random songs from the server.
Returns a list of N `listen-track's."
  (mapcar #'listen-subsonic--json-to-listen
          (infrasonic-get-random-songs (listen-subsonic--client) n)))

;;;; Add to queue functions

(transient-define-prefix listen-subsonic-queue-menu ()
  "Queue tracks from Subsonic."
  :info-manual "(listen) Subsonic Queue"
  ["Queue from Subsonic"
   ["Albums"
    ("n" "New releases" listen-subsonic-queue-recent-release)
    ("m" "Most played" listen-subsonic-queue-most-played)
    ("r" "Recently listened" listen-subsonic-queue-recent-play)
    ("*" "Starred albums" listen-subsonic-queue-starred-album)]
   ["Songs"
    ("p" "Random songs" listen-subsonic-queue-random)
    ("S" "Starred tracks" listen-subsonic-queue-starred-tracks)
    ("l" "Playlist" listen-subsonic-queue-playlist)
    ("s" "Search" listen-subsonic-queue-search)]
   ["Manage playlists"
    ("P" "Create playlist" listen-subsonic-create-playlist)
    ("D" "Delete playlist" listen-subsonic-delete-playlist)
    ("U" "Update playlist" listen-subsonic-update-playlist)
    ("R" "Rename playlist" listen-subsonic-rename-playlist)]])

(defun listen-subsonic-queue-random (n queue)
  "Add N random songs to QUEUE."
  (interactive
   (list
    (read-number "Number of songs: " 10)
    (listen-queue-complete :allow-new-p t)))
  (listen-queue-add-tracks (listen-subsonic-get-random-tracks n) queue))

(defun listen-subsonic-queue-playlist (queue)
  "Prompt for a playlist and add its tracks to QUEUE."
  (interactive (list (listen-queue-complete :allow-new-p t)))
  (let* ((id (listen-subsonic--read-playlist))
         (tracks (listen-subsonic--get-playlist-tracks id)))
    (listen-queue-add-tracks tracks queue)))

(defun listen-subsonic-queue-starred-tracks (queue)
  "Fetch all starred tracks and add them to QUEUE."
  (interactive (list (listen-queue-complete :allow-new-p t)))
  (listen-queue-add-tracks (listen-subsonic-get-starred-tracks)
                           queue))

;; Queue from a list of albums

(defun listen-subsonic--queue-album-from-list (queue type &optional prompt sort-fn)
  "Add an album from TYPE list to QUEUE.

TYPE is passed to `infrasonic-get-album-list', and may be:
- A genre string, for example: \"Rock\".
- `:random': Random albums.
- `:newest': Newest albums by release date.
- `:frequent': User's most frequently played albums.
- `:recent': Recently added albums.
- `:starred': Starred albums.
- `:byname': Alphabetically sorted by name.
- `:byartist': Alphabetically sorted by artist."
  (let* ((type (or type (error "Type must be non-nil")))
         (client (listen-subsonic--client))
         (albums (infrasonic-get-album-list client type))
         (album (listen-subsonic--read-album albums prompt sort-fn))
         (tracks (listen-subsonic--get-all-tracks (alist-get 'id album) :album)))
    (listen-queue-add-tracks tracks queue)))

(defun listen-subsonic-queue-recent-release (queue)
  "Add a recently released album to QUEUE."
  (interactive (list (listen-queue-complete :allow-new-p t)))
  ;; Sort by year, then alphabetically
  (let ((sort-comp
         (lambda (a b)
           (let* ((ya (or (alist-get 'year a) 0))
                  (yb (or (alist-get 'year b) 0))
                  (ya (if (stringp ya) (string-to-number ya) ya))
                  (yb (if (stringp yb) (string-to-number yb) yb)))
             (cond
              ;; Different year -> numeric
              ((/= ya yb) (> ya yb))
              ;; Same year -> alphabetical
              (t (string-lessp (or (alist-get 'name a) "")
                               (or (alist-get 'name b) ""))))))))
    (listen-subsonic--queue-album-from-list queue
                                            :newest
                                            "Recently released albums: "
                                            sort-comp)))

(defun listen-subsonic-queue-most-played (queue)
  "Add a frequently played album to QUEUE."
  (interactive (list (listen-queue-complete :allow-new-p t)))
  (let ((sort-comp
         (lambda (a b)
           (let* ((ca (or (alist-get 'playCount a) 0))
                  (cb (or (alist-get 'playCount b) 0))
                  (ca (if (stringp ca) (string-to-number ca) ca))
                  (cb (if (stringp cb) (string-to-number cb) cb)))
             (cond
              ;; Different year -> numeric
              ((/= ca cb) (> ca cb))
              ;; Same year -> alphabetical
              (t (string-lessp (or (alist-get 'name a) "")
                               (or (alist-get 'name b) ""))))))))
    (listen-subsonic--queue-album-from-list queue
                                            :frequent
                                            "Frequently played albums: "
                                            sort-comp)))

(defun listen-subsonic-queue-recent-play (queue)
  "Add a recently played album to QUEUE."
  (interactive (list (listen-queue-complete :allow-new-p t)))
  (let ((sort-comp
         (lambda (a b)
           (let* ((ta (float-time (date-to-time (or (alist-get 'created a) 0))))
                  (tb (float-time (date-to-time (or (alist-get 'created b) 0)))))
             (> ta tb)))))
    (listen-subsonic--queue-album-from-list queue
                                            :recent
                                            "Recently played albums: "
                                            sort-comp)))

(defun listen-subsonic-queue-starred-album (queue)
  "Add a starred album to QUEUE."
  (interactive (list (listen-queue-complete :allow-new-p t)))
  (let ((sort-comp
         (lambda (a b)
           (let* ((sa (float-time (date-to-time (or (alist-get 'starred a) 0))))
                  (sb (float-time (date-to-time (or (alist-get 'starred b) 0)))))
             (> sa sb)))))
    (listen-subsonic--queue-album-from-list queue
                                            :starred
                                            "Starred albums: "
                                            sort-comp)))

;;;; Library view
;; This is annoying for a few reasons. If a library is massive, it may take minutes to generate a
;; full library. So we generate a taxy view of just artists, and then a proper listen-library view
;; of the artist's albums and songs.

(defvar listen-subsonic-library--artists-library "*Listen Subsonic Artists*")

(defvar-keymap listen-subsonic-library-artists-mode-map
  :parent magit-section-mode-map
  "RET" #'listen-subsonic-library-open-artist
  "g" #'listen-subsonic-library)

(define-derived-mode listen-subsonic-library-artists-mode magit-section-mode "Listen-Subsonic-Artists"
  "Browse artists on your OpenSubsonic server.")

(defun listen-subsonic-library--artist-index-key (artist)
  "Group ARTIST by first letter.

The OpenSubsonic API returns artists indexed by first letter,
categorising into A-Z, or symbols in #."
  (let* ((name (or (alist-get 'name artist) ""))
         (first (if (> (length name) 0) (downcase (substring name 0 1)) "#")))
    (if (string-match-p "^[a-z]$" first) first "#")))

(defun listen-subsonic-library--format-artist (artist)
  "Return library display string for ARTIST.

Shows artist name and number of albums. Gives \"[unknown artist]\" to
artists with missing names."
  (let ((name (or (alist-get 'name artist) "[unknown artist]"))
        (albums (or (alist-get 'albumCount artist) 0)))
    (format "%s  (%s albums)" name albums)))

;;;###autoload
(defun listen-subsonic-library ()
 "Open a library view of all Subsonic artists.

`RET' opens that artist in an actual `listen-library' library view."
  (interactive)
  (let* ((client (listen-subsonic--client))
         (artists (infrasonic-get-artists-flat client))
         (format-fn #'listen-subsonic-library--format-artist)
         (make-fn)
         (taxy))
    (setq make-fn
          (lambda (&rest args)
            (apply #'make-taxy-magit-section
                   :make make-fn
                   :format-fn format-fn
                   args)))
    (setq taxy
          (funcall make-fn
                   :name "Artists"
                   :take (apply-partially #'taxy-take-keyed
                                          (list #'listen-subsonic-library--artist-index-key))))
    (with-current-buffer (get-buffer-create listen-subsonic-library--artists-library)
      (listen-subsonic-library-artists-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (taxy-magit-section-insert
          (taxy-sort* #'string< #'taxy-name
            (taxy-fill artists (taxy-emptied taxy))))
        (goto-char (point-min)))
      (pop-to-buffer (current-buffer)))))

(defun listen-subsonic-library--artist-at-point ()
  "Return artist alist at point within an Artists taxy library.

Used to get the artist the user selected, and should be passed to
`listen-subsonic-library-open-artist'."
  (when-let ((sec (magit-current-section))
             (val (oref sec value)))
    (when (and (listp val)
               (eq (alist-get 'subsonic-type val) :artist))
      val)))

;;;###autoload
(defun listen-subsonic-library-open-artist (&optional artist)
  "Open selected ARTIST's albums/songs in an actual `listen-library'."
  (interactive)
  (let* ((artist (or artist (listen-subsonic-library--artist-at-point)))
         (client (listen-subsonic--client)))
    (unless artist
      (user-error "No artist at point"))
    (let* ((artist-id (alist-get 'id artist))
           (artist-name (or (alist-get 'name artist) "Subsonic Artist")))
      (unless artist-id
        (user-error "Artist has no id"))
      (let* ((songs (infrasonic-get-all-songs client artist-id :artist))
             (tracks (mapcar (lambda (s) (listen-subsonic--json-to-listen s client))
                             songs)))
        (listen-library tracks :name (format "Subsonic: %s" artist-name))))))

;;;; Search

(defun listen-subsonic--search-select (query)
  "Search for QUERY and prompt user to select results.
Returns a list of `listen-track's for the selected item.

The user selects from a mixed list of artists, albums, and songs.
Selecting an artist or album expands it to all its songs."
  (let* ((client (listen-subsonic--client))
         (results (infrasonic-search client query))
         (entries (mapcar (lambda (item)
                           (let* ((type (alist-get 'subsonic-type item))
                                  (name (alist-get 'name item))
                                  (disp (pcase type
                                          ;; Think of a better indicator...
                                          (:artist (format "🎸 %s" name))
                                          (:album (format "💿 %s" name))
                                          (:song (format "🎵 %s" name)))))
                             (cons disp item)))
                         results))
         (affix-fn (listen-subsonic--affixation
                    entries
                    #'listen-subsonic--item-suffix nil
                    #'listen-subsonic--item-prefix nil))
         (extra-metadata `((affixation-function . ,affix-fn)
                           (display-sort-function . identity)
                           (cycle-sort-function . identity)))
         (selected (listen-subsonic--completing-read
                    (format "Search results for \"%s\": " query)
                    entries extra-metadata))
         (type (alist-get 'subsonic-type selected))
         (id (alist-get 'id selected)))
    (pcase type
      (:song (list (listen-subsonic--json-to-listen selected client)))
      (:album (listen-subsonic--get-all-tracks id :album))
      (:artist (listen-subsonic--get-all-tracks id :artist)))))

(defun listen-subsonic-queue-search (query queue)
  "Search for QUERY and add selected results to QUEUE."
  (interactive
   (list (read-string "Search Subsonic: ")
         (listen-queue-complete :allow-new-p t)))
  (let ((tracks (listen-subsonic--search-select query)))
    (if tracks
        (listen-queue-add-tracks tracks queue)
      (user-error "No results for \"%s\"" query))))

(defun listen-subsonic-library-search (query)
  "Search for QUERY and show selected results in a `listen-library' view."
  (interactive (list (read-string "Search Subsonic: ")))
  (let ((tracks (listen-subsonic--search-select query)))
    (if tracks
        (listen-library tracks :name (format "Subsonic search: %s" query))
      (user-error "No results for \"%s\"" query))))

;;;; Cover art

;; TODO: slightly buggy, 2 images flash on screen before settling to 1
;; TODO: might move this to infrasonic.el
(defun listen-subsonic--cover-art-path (track-id)
  "Return the local cache path for cover art of TRACK-ID."
  (expand-file-name (format "art-%s.jpg" track-id)
                    listen-subsonic-cache-dir))

(defun listen-subsonic--ensure-cover-art (track callback &optional size)
  "Ensure cover art for TRACK is cached, then call CALLBACK with the file path.
SIZE overrides the default art size.  CALLBACK receives the path
to the cached image file."
  (let* ((etc (listen-track-etc track))
         (id (alist-get 'id etc)))
    (when id
      (let ((path (listen-subsonic--cover-art-path id)))
        (if (file-exists-p path)
            (funcall callback path)
          (infrasonic-download-art (listen-subsonic--client)
                                  id path size
                                  callback))))))

(defun listen-subsonic--insert-cover-art (track &optional size)
  "Insert cover art for TRACK into the current buffer.
SIZE is the pixel edge length (defaults to 128).
Caches image descriptors in `listen-subsonic--image-cache' so
repeated calls (e.g. the 1-second status buffer timer) avoid
re-reading from disk."
  (let* ((size (or size 128))
         (id (alist-get 'id (listen-track-etc track)))
         (cache-key (cons id size))
         (cached-image (gethash cache-key listen-subsonic--image-cache))
         (buffer (current-buffer))
         (marker (copy-marker (point))))
    (if cached-image
        ;; cache hit
        (progn
          (insert-image cached-image " ")
          (insert "\n"))
      ;; miss, read and then cache
      (listen-subsonic--ensure-cover-art
       track
       (lambda (path)
         (when (and (buffer-live-p buffer)
                    (file-exists-p path))
           (let ((image (create-image path nil nil
                                      :width size :height size
                                      :ascent 'center)))
             (puthash cache-key image listen-subsonic--image-cache)
             (with-current-buffer buffer
               (let ((inhibit-read-only t))
                 (save-excursion
                   (goto-char marker)
                   (insert-image image " ")
                   (insert "\n")))))))
       size))))

;;;; Rating

(defun listen-subsonic-rate-track (track rating)
  "Set TRACK's rating to RATING (0-5).
When called interactively, rate the currently playing track.
RATING of 0 removes the rating."
  (interactive
   (let ((track (listen-current-track)))
     (unless track
       (user-error "No track playing"))
     (list track (read-number "Rating (0-5): "
                              (if-let ((r (listen-track-rating track)))
                                  (round (* 5 (string-to-number r)))
                                0)))))
  (unless (and (integerp rating) (<= 0 rating 5))
    (user-error "Rating must be 0-5"))
  (when-let* ((id (alist-get 'id (listen-track-etc track))))
    (infrasonic-set-rating
     (listen-subsonic--client) id rating
     (lambda (_)
       (setf (listen-track-rating track)
             (if (zerop rating) nil
               (format "%f" (/ rating 5.0))))
       (message "Rated '%s' %s/5"
                (listen-track-title track) rating)))))

;;;; Playlists

(defun listen-subsonic-delete-playlist ()
  "Delete a Subsonic playlist selected with completion."
  (interactive)
  (let* ((playlists (infrasonic-get-playlists (listen-subsonic--client)))
         (name (completing-read "Delete playlist: " playlists nil t))
         (id (alist-get name playlists nil nil #'equal)))
    (when (yes-or-no-p (format "Really delete playlist \"%s\"? " name))
      (infrasonic-delete-playlist (listen-subsonic--client) id)
      (message "Deleted playlist \"%s\"" name))))

(defun listen-subsonic-update-playlist (queue)
  "Update a Subsonic playlist with tracks from QUEUE.
Only Subsonic-sourced tracks in QUEUE will be included.
The playlist's track list is replaced entirely."
  (interactive (list (listen-queue-complete)))
  (let* ((playlists (infrasonic-get-playlists (listen-subsonic--client)))
         (name (completing-read "Update playlist: " playlists nil t))
         (id (alist-get name playlists nil nil #'equal))
         (ids (mapcan (lambda (track)
                        (let ((etc (listen-track-etc track)))
                          (when (equal (alist-get 'source etc) "subsonic")
                            (list (alist-get 'id etc)))))
                      (listen-queue-tracks queue))))
    (if ids
        (progn
          (infrasonic-update-playlist (listen-subsonic--client) id ids)
          (message "Updated playlist \"%s\" with %d tracks" name (length ids)))
      (user-error "No Subsonic tracks found in queue"))))

(defun listen-subsonic-rename-playlist ()
  "Rename a Subsonic playlist."
  (interactive)
  (let* ((playlists (infrasonic-get-playlists (listen-subsonic--client)))
         (old-name (completing-read "Rename playlist: " playlists nil t))
         (id (alist-get old-name playlists nil nil #'equal))
         (new-name (read-string (format "Rename \"%s\" to: " old-name) old-name))
         (songs (infrasonic-get-playlist-songs (listen-subsonic--client) id))
         (song-ids (mapcar (lambda (s) (alist-get 'id s)) songs)))
    (infrasonic-update-playlist (listen-subsonic--client) id song-ids new-name)
    (message "Renamed playlist to \"%s\"" new-name)))

(provide 'listen-subsonic)

;;; listen-subsonic.el ends here
