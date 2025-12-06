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
(require 'plz)          ; HTTP requests
(require 'auth-source)  ; authinfo
(require 'listen-queue) ; Add tracks to queue

(require 'map)          ; for map-let and map-elt
(require 'url-util)     ; for url-build-query-string

;; Declares

(declare-function listen-library "listen-library")

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

(defvar listen-subsonic-cache-dir (expand-file-name "listen.el" temporary-file-directory)
  "Directory to store cached subsonic data.")

(defvar listen-subsonic--art-queue nil
  "Queue for art downloads in browser.")

(defvar listen-subsonic--art-active 0
  "Number of active downloads.")

(defvar listen-subsonic--art-max 10
  "Max concurrent downloads.")

;;;; General helpers

(defun listen-subsonic--ensure-list (item)
  "Ensure ITEM is a list."
  (if (vectorp item)
      (append item nil)
    ;; This already exists?
    (ensure-list item)))

;;;; Auth helpers

(defun listen-subsonic--get-credentials ()
  "Fetch user credentials securely from `auth-source`."
  (let ((auth (auth-source-search :host listen-subsonic-url)))
    (when auth
      (car auth))))

(defun listen-subsonic--get-auth-params ()
  "Return auth info alist for API calls."
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

;; TODO: Maybe allow insecure http later?
(defun listen-subsonic--build-url (endpoint params)
  "Build a Subsonic API URL from ENDPOINT and PARAMS."
  (let* ((param-list (mapcar (lambda (p)
                               (list (car p) (cdr p)))
                             params))
         (param-str (url-build-query-string param-list nil t)))
    (format "https://%s/rest/%s.view?%s"
            listen-subsonic-url endpoint param-str)))

;;;; API Helpers

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
    (user-error "Please set `listen-subsonic-url'"))
  (let* ((api-params (append (listen-subsonic--get-auth-params) params))
         (api-url (listen-subsonic--build-url endpoint api-params))
         (api-headers '(("Accept-Encoding" . "gzip"))))
    (plz 'get api-url
      :headers api-headers
      :as #'listen-subsonic--process-api-response
      :then (or callback 'sync))))

;;;; Data formatting

(defun listen-subsonic--get-stream-url (id &optional auth-params)
  "Return a URL for MPV to directly stream track with ID.
If AUTH-PARAMS is nil, new auth params are generated."
  (listen-subsonic--build-url
   "stream"
   (append (or auth-params (listen-subsonic--get-auth-params))
           `(("id" . ,id)))))

(defun listen-subsonic--json-to-listen (s &optional auth-params)
  "Convert JSON alist S into a `listen-track' structure.
If AUTH-PARAMS is nil, new auth params are generated."
  (map-let (('id id) ('userRating rating) artist title album track genre duration year starred) s
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
     :metadata s
     :etc `((source . "subsonic")
            (id . ,id)
            (starred . ,(when starred t))))))

;;;; Read requests

(defun listen-subsonic--get-items (endpoint rootkey itemkey &optional params)
  "Call ENDPOINT and get the contents of ROOTKEY, then ITEMKEY.
PARAMS are optional API parameters."
  (let* ((response (listen-subsonic--api-call endpoint params))
         (root (alist-get rootkey response))
         (items (alist-get itemkey root)))
    (listen-subsonic--ensure-list items)))

;; TODO: This blocks emacs while waiting for response
;; Look into consult's async features at some
;; stage? Maybe not necessary but will allow searching way more than 50
(defun listen-subsonic-search-tracks (query)
  "Return a list of `listen-track' objects.
Uses the Subsonic API's \"search3\" endpoint with QUERY as the search query.
The maximum returned tracks is 50."
  (let ((items (listen-subsonic--get-items
                "search3" 'searchResult3 'song
                `(("query" . ,query)
                  ("songCount" . ,listen-subsonic-search-max-results)))))
    (mapcar #'listen-subsonic--json-to-listen items)))

(defun listen-subsonic-get-starred-tracks ()
  "Fetch all starred songs from Subsonic server."
  (let ((items (listen-subsonic--get-items
                "getStarred" 'starred 'song)))
    (mapcar #'listen-subsonic--json-to-listen items)))

(defun listen-subsonic--get-playlists ()
  "Return an alist (name . id) of all playlists accessible to the user."
  (let ((items (listen-subsonic--get-items
                "getPlaylists" 'playlists 'playlist)))
    (mapcar (lambda (item)
              (cons (alist-get 'name item)
                    (format "%s" (alist-get 'id item))))
            items)))

(defun listen-subsonic--get-playlist-tracks (playlist)
  "Return all tracks in PLAYLIST."
  (let ((items (listen-subsonic--get-items
                "getPlaylist" 'playlist 'entry
                `(("id" . ,playlist)))))
    (mapcar #'listen-subsonic--json-to-listen items)))

;; TODO: merge this with get-folder-tracks to simplify browse code
(defun listen-subsonic--get-all-tracks (id)
  "Fetch all tracks under directory ID recursively."
  (let* ((data (listen-subsonic--api-call "getMusicDirectory" `(("id" . ,id))))
         (parent (alist-get 'directory data))
         (children (alist-get 'child parent)))
    (mapcan (lambda (c)
              (if (alist-get 'isDir c)
                  (listen-subsonic--get-all-tracks (alist-get 'id c))
                (list (listen-subsonic--json-to-listen c
                                                       (listen-subsonic--get-auth-params)))))
            children)))

(defun listen-subsonic--get-folder-tracks (id)
  "Return all tracks in music folder ID."
  (let ((items (listen-subsonic--get-items
                "search3" 'searchResult3 'song
                `(("musicFolderId" . ,id)
                  ("query" . "")
                  ("songCount" . "100000")))))
    (mapcar #'listen-subsonic--json-to-listen items)))

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
  "Notifies the Subsonic server that we have started playing a track in PLAYER.
Should be added to `listen-track-start-functions'."
  (listen-subsonic--scrobble player nil))

(defun listen-subsonic-scrobble-end (player)
  "Notifies the Subsonic server that we have finished a track in PLAYER.
Should be added to `listen-track-end-functions'."
  (listen-subsonic--scrobble player t))

;;;; Server browsing functions

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

(defun listen-subsonic--flatten-indexes (data)
  "Get a flat list of artists from DATA, which is JSON returned by the \"getIndexes\" endpoint."
  (let ((idxs (listen-subsonic--ensure-list (map-nested-elt data '(indexes index)))))
    (mapcan (lambda (idx)
              (let ((artists (listen-subsonic--ensure-list
                              (alist-get 'artist idx))))
                (mapcar (lambda (a) (cons '(isDir . t) a)) artists)))
            idxs)))

(defun listen-subsonic--browse-next-level (level)
  "Return the next level under LEVEL."
  (pcase level
    (:root :indexes)
    (:indexes :directory)
    (_ :directory)))

(defun listen-subsonic--browse-get-prefix (item)
  "Return a fixed-width string of ls-like metadata for ITEM."
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
  "Ping the server to check connectivity and authentication."
  (interactive)
  (if (listen-subsonic--api-call "ping")
      (message "Successfully pinged Subsonic server!")
    (message "Failed to ping server.")))

(defun listen-subsonic--queue-tracks (tracks queue)
  "Add TRACKS to QUEUE with a message."
  (if tracks
      (progn
        (listen-queue-add-tracks tracks queue)
        (message "Added %d tracks to queue '%s'."
                 (length tracks) (listen-queue-name queue))
        (listen-queue queue))
    (message "No tracks found.")))

(defun listen-subsonic-queue-random (n queue)
  "Fetch and add to QUEUE a list of N random songs."
  (interactive
   (list
    (read-number "Number of songs: " 10)
    (listen-queue-complete :allow-new-p t)))
  (let* ((items (listen-subsonic--get-items
                 "getRandomSongs" 'randomSongs 'song
                 `(("size" . ,(number-to-string n)))))
         (tracks (mapcar #'listen-subsonic--json-to-listen items)))
    (listen-subsonic--queue-tracks tracks queue)))

;; TODO: Deduplicate logic between this and the library code below
(defun listen-subsonic-queue-playlist (queue)
  "Add all tracks from a user's playlist to the QUEUE."
  (interactive (list (listen-queue-complete :allow-new-p t)))
  (let* ((playlists (listen-subsonic--get-playlists))
         (name (completing-read "Playlist: " playlists nil t))
         (id (alist-get name playlists nil nil #'equal))
         (tracks (listen-subsonic--get-playlist-tracks id)))
    (listen-subsonic--queue-tracks tracks queue)))

(defun listen-subsonic-queue-starred-tracks (queue)
  "Add all starred songs from Subsonic server to QUEUE."
  (interactive (list (listen-queue-complete :allow-new-p t)))
  (let ((tracks (listen-subsonic-get-starred-tracks)))
    (listen-subsonic--queue-tracks tracks queue)))

;; TODO: C-u adds to start of queue/next?
;; TODO; Use annotate-function to make this (and other functions) look better
(defun listen-subsonic-queue-search-tracks (query queue)
  "Search Subsonic server for QUERY and add results to the current QUEUE."
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

(defun listen-library-from-subsonic (&optional source)
  "Show a library view for subsonic.
SOURCE may be one of:
- \"Browse\": Allows the user to browse a directory tree.
- \"Starred Tracks\": Library from starred tracks.
- \"Playlist\": Library from a playlist.
- \"Search\": Library from a search query."
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

(defun listen-subsonic-clear-cache ()
  "Clear the Subsonic cache directory."
  (interactive)
  (when (file-exists-p listen-subsonic-cache-dir)
    (delete-directory listen-subsonic-cache-dir t))
  (message "Cleared Subsonic cache."))

;; Completing read browser
(defun listen-subsonic-browse-library ()
  "Browse the Subsonic library hierarchy using `completing-read'.
Library hierarchy: Folder -> Artist -> Album -> Song.
Select the \"[All]\" option to select all tracks under the current level."
  (interactive)
  (let ((result (listen-subsonic--browse-step :root nil "Root")))
    (when result
      (if (called-interactively-p 'interactive)
          (listen-library (nth 0 result) :name (nth 1 result))
        (funcall (nth 0 result))))))

(defun listen-subsonic--browse-step (level id name &optional history)
  "Enter LEVEL defined by ID with NAME.
Backend for `listen-subsonic-browse-library'. HISTORY contains the user's navigation history."
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

;; dired-like browser UI
(defvar listen-subsonic-browse-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "^") #'listen-subsonic--browse-up)
    (define-key map (kbd "g") #'revert-buffer)
    (define-key map (kbd "A") #'listen-subsonic--browse-add-all)
    map)
  "Keymap for `listen-subsonic-browse-mode'.")

(define-derived-mode listen-subsonic-browse-mode special-mode "Subsonic-Browser"
  "Major mode for `dired'-like browsing of Subsonic libraries."
  :interactive nil
  :keymap listen-subsonic-browse-mode-map
  (setq-local revert-buffer-function #'listen-subsonic--browse-revert
              listen-subsonic--browse-history nil
              listen-subsonic--browse-current-id nil
              listen-subsonic--browse-current-name nil
              listen-subsonic--browse-current-level nil))

;; TODO: Restrict point to NOT enter prefix (similar to dired)
(defun listen-subsonic-browse ()
  "Open a `dired'-like Subsonic browser buffer."
  (interactive)
  (let ((buf (get-buffer-create "*Listen Subsonic Browser*")))
    (with-current-buffer buf
      (listen-subsonic-browse-mode)
      (listen-subsonic--browse-render nil "Root" :root)) ;; Start at :root
    (switch-to-buffer buf)))

;; TODO: cl-decf and cl-incf are built-in in emacs 31.1 (decf and incf)
(defun listen-subsonic--process-art-queue ()
  "Process background art queue."
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

(defun listen-subsonic--browse-fetch-art (id buf pos)
  "Fetch cover art for ID and display it a POS in BUF."
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
  "Display FILE's image in BUF at POS."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (with-silent-modifications
        (let ((image (create-image file nil nil
                                  :ascent 'center
                                  :height 64)))
          (put-text-property pos (1+ pos) 'display image))))))

;; Render the "dired" buffer
(defun listen-subsonic--browse-insert-item (item next)
  "Insert a single ITEM with link to NEXT level into the listen browser buffer."
  (let* ((dirp (alist-get 'isDir item))
         (name (alist-get 'name item))
         (prefix (listen-subsonic--browse-get-prefix item))
         (pt (point))
         (face (if dirp
                   (pcase listen-subsonic--browse-current-level
                     (:root 'listen-genre)
                     (:indexes 'listen-artist)
                     (t 'listen-album))
                 'listen-title)))
    (insert (propertize prefix 'face 'shadow))
    (insert-text-button
     (concat (if dirp "📁 " "🎵 ") name)
     'action #'listen-subsonic--browse-button
     'follow-link t
     'subsonic-item item
     'subsonic-next (if dirp next nil)
     'face face)
    (insert "\n")
    ;; returns id and pos for art
    (list (or (alist-get 'coverArt item) (alist-get 'id item))
          (current-buffer)
          (+ pt (length prefix)))))

(defun listen-subsonic--browse-render (id name level)
  "Display a view for LEVEL (folder/artist/album) of ID and NAME."
  (setq-local listen-subsonic--browse-current-id id
              listen-subsonic--browse-current-name name
              listen-subsonic--browse-current-level level)
  (let* ((inhibit-read-only t)
         (items (listen-subsonic--get-nodes level id))
         (next (listen-subsonic--browse-next-level level)))

    ;; prepare buffer
    (erase-buffer)
    ;; "path" of current level
    (let* ((parents (mapcar (lambda (h) (nth 1 h)) listen-subsonic--browse-history))
           (path (reverse (cons name parents))))
      (insert (propertize (string-join path " / ") 'face 'dired-header) "\n"))
    ;; "." to revert buffer/refresh
    ;; ".." to go up (same as "^" bind)
    (insert-text-button "."
                        'action (lambda (_) (revert-buffer))
                        'follow-link t
                        'face 'dired-directory)
    (insert "\n")
    (when listen-subsonic--browse-history
      (insert-text-button ".."
                          'action (lambda (_) (listen-subsonic--browse-up))
                          'follow-link t
                          'face 'dired-directory)
      (insert "\n"))
    (let ((next (listen-subsonic--browse-next-level level)))
      (dolist (item (listen-subsonic--get-nodes level id))
        (pcase-let ((`(,art-id ,buf ,pos)
                     (listen-subsonic--browse-insert-item item next)))
          (when (and art-id (not (memq level '(:root :indexes))))
            (listen-subsonic--browse-fetch-art art-id buf pos)))))))

;; browser functions

(defun listen-subsonic--browse-add-all ()
  "Add all tracks in/under the current view to the current queue."
  (interactive)
  (let ((tracks (pcase listen-subsonic--browse-current-level
                  (:indexes
                   (listen-subsonic--get-folder-tracks listen-subsonic--browse-current-id))
                  (:directory
                   (listen-subsonic--get-all-tracks listen-subsonic--browse-current-id))
                  (_
                   (user-error "Cannot add all tracks under the current view or the root level")))))
    (when tracks
      (listen-queue-add-tracks tracks (listen-queue-complete))
      (message "Added %d tracks to the queue." (length tracks)))
    (message "No tracks found.")))

(defun listen-subsonic--browse-button (&optional button)
  "Activate the text BUTTON at point.
If button at point is a directory, it will enter and redisplay the buffer.
If button at point is a song, it will add it to the current queue."
  (interactive)
  (let* ((pt (if button (button-start button) (point)))
         (item (get-text-property pt 'subsonic-item))
         (next (get-text-property pt 'subsonic-next)))
    (unless item (user-error "No item at point"))

    ;; open directory
    (if (alist-get 'isDir item)
        (progn
          ;; add current state to history
          (push (list listen-subsonic--browse-current-id
                      listen-subsonic--browse-current-name
                      listen-subsonic--browse-current-level)
                listen-subsonic--browse-history)
          ;; display next level
          (listen-subsonic--browse-render (alist-get 'id item)
                                          (or (alist-get 'title item) (alist-get 'name item))
                                          next))
      ;; song
      (listen-queue-add-tracks (list (listen-subsonic--json-to-listen item))
                               (listen-queue-complete))
      (message "Added '%s' to queue." (alist-get 'title item)))))

(defun listen-subsonic--browse-up ()
  "Go up a level in the Subsonic directory structure."
  (interactive)
  (if-let ((prev (pop listen-subsonic--browse-history)))
      (listen-subsonic--browse-render (nth 0 prev) (nth 1 prev) (nth 2 prev))
    (message "This is the highest level.")))

(defun listen-subsonic--browse-revert (_ignore-auto _noconfirm)
  "Custom revert function for Listen-Browser buffers."
  (listen-subsonic--browse-render listen-subsonic--browse-current-id
                                  listen-subsonic--browse-current-name
                                  listen-subsonic--browse-current-level))

(provide 'listen-subsonic)
;;; listen-subsonic.el ends here
