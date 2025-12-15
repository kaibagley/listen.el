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

;; TODO: Move api calls to infrasonic
;; TODO: Some kind of indicator to show if track is starred or not
;; TODO: Send bookmark request to server periodically
;; TODO: When emacs 31.1 is released, cl-decf/cl-incf -> decf/incf

(require 'infrasonic)   ; For Subsonic backend
(require 'listen-queue) ; Add tracks to queue
(require 'svg-lib)      ; For starred icon

;; Declares

(declare-function listen-library "listen-library")

;;;; Customisation

(defgroup listen-subsonic nil
  "`listen' options for Subsonic backend."
  :group 'listen)

;; Users set infrasonic variables for URL, protocol, etc.

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

(defvar listen-subsonic--menu-max-width 50
  "Maximum width of strings returned by search function.")

;;;; General helpers

(defun listen-subsonic--json-to-listen (json-data)
  "Convert an `infrasonic' JSON-DATA into a `listen-track'.
Returns a `listen-track' struct."
  (map-let
      (('id id) ('userRating rating) artist title album track genre duration year starred)
      json-data
    (make-listen-track
     :filename (infrasonic-get-stream-url id) ; silly mpv
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

(defun listen-subsonic-search-tracks (query)
  "Search the server for tracks matching QUERY.
Returns a list of `listen-track's.

Uses the Subsonic API's \"search3\" endpoint with QUERY as the search query."
  (mapcar #'listen-subsonic--json-to-listen
          (infrasonic-search-tracks query)))

(defun listen-subsonic-get-starred-tracks ()
  "Fetch all starred songs from the server.
Returns a list of `listen-track's."
  (mapcar #'listen-subsonic--json-to-listen
          (infrasonic-get-starred-tracks)))

(defun listen-subsonic--get-playlist-tracks (id)
  "Fetch all tracks in playlist with ID.
Returns a list of `listen-track's."
  (mapcar #'listen-subsonic--json-to-listen
          (infrasonic-get-playlist-tracks id)))

(defun listen-subsonic--get-all-tracks (id &optional level)
  "Fetch all tracks under item associated with ID.
Returns a list of `listen-track's.

LEVEL determines what level of the hierarchy we are on:
- :artist: fetches all albums, then all songs by that artist.
- :album: fetches all songs on the album."
  (mapcar #'listen-subsonic--json-to-listen
          (infrasonic-get-all-tracks id level)))

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
        (infrasonic-create-playlist ids name)
      (user-error "No Subsonic tracks found"))))

(defun listen-subsonic--scrobble (player submission-p)
  "Scrobble the current track playing in PLAYER's queue to the Subsonic API.
Returns the unparsed API response.

When SUBMISSION-P is non-nil, server is notified that the currently playing track is finished.
When SUBMISSION-P is nil, server is notified the current tracks is \"now playing\"."
  (when-let* ((queue (map-elt (listen-player-etc player) :queue))
              (track (listen-queue-current queue))
              (source (equal (map-elt (listen-track-etc track) 'source) "subsonic"))
              (id (alist-get 'id (listen-track-etc track))))
    (infrasonic-scrobble id submission-p)))

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
Normalises artists, albums and tracks such that:
- All three have the alist elements \"name\" and \"subsonic-type\".
- Artists and albums have alist element \"isDir\".

LEVEL determines the endpoint to use, and may be one of:
- :artists: Returns top-level view of all artists using endpoint \"getArtists\".
- :artist: Returns albums for an artist using \"getArtist\".
- :album: Returns songs in an album using \"getAlbum\"."
  (let ((items
         (pcase level
           (:artists (infrasonic-get-artists))
           (:artist (infrasonic-get-artist id))
           (:album (infrasonic-get-album id)))))
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
- Starred song: \"- * 2004  3:43\"
- Artist:       \"d - ---- 53:19\"
- Album:        \"d - 2004 --:--\""
  (let* ((dirp (alist-get 'isDir item))
         (year (alist-get 'year item))
         (duration (alist-get 'duration item))
         (starred (alist-get 'starred item)))
    (format "%s %s %4s %5s "
            (if dirp "d" "-")
            (if starred "*" "-")
            (if year (number-to-string year) "----")
            (if duration (listen-format-seconds duration) "--:--"))))

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
    (infrasonic-star id star-p
                     ;; update track in-memory
                     (lambda (_)
                       (setf (alist-get 'starred (listen-track-etc track)) star-p)
                       (message "%s '%s'" (if star-p "Starred" "Unstarred")
                                (listen-track-title track))))))

(defun listen-subsonic--completing-read (prompt entries &optional extra-metadata)
  "Read a candidate with PROMPT from ENTRIES.
Returns the chosen item.

ENTRIES is an alist of display strings, and its corresponding value ((disp-str . item) ...).
EXTRA-METADATA is an alist of completion metadata pairs for `completing-read', to be `cons'ed with
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
           (let* ((len (string-width cand))
                  (padding (make-string (- listen-subsonic--menu-max-width len) ?\s))
                  (suf (if suffix-fn (funcall suffix-fn item) ""))
                  (suffix (if suffix-face (propertize suf 'face suffix-face) suf))
                  (pre (if prefix-fn (funcall prefix-fn item) ""))
                  (prefix (if prefix-face (propertize pre 'face prefix-face) pre)))
             (list cand prefix (concat padding suffix))))))
     cands)))

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
     (format "%s albums" (or (alist-get 'albumCount item) 0)))
    (:album
     (concat (listen-subsonic--format-column (alist-get 'artist item)
                                             12 'listen-artist)
             " "
             (when-let* ((year (alist-get 'year item)))
               (format "(%s)" year))))
    (:track
     (concat (listen-subsonic--format-column (alist-get 'artist item)
                                             12 'listen-artist)
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

(defun listen-subsonic-get-random-tracks (n)
  "Fetch N random songs from the server.
Returns a list of N `listen-track's."
  (mapcar #'listen-subsonic--json-to-listen
          (infrasonic-get-random-tracks)))

(defun listen-subsonic-queue-random (n queue)
  "Add N random songs to QUEUE."
  (interactive
   (list
    (read-number "Number of songs: " 10)
    (listen-queue-complete :allow-new-p t)))
  (listen-queue-add-tracks (listen-subsonic-get-random-tracks n) queue))

(defun listen-subsonic--read-playlist ()
  "Prompt user to select a Subsonic playlist using `completing-read'.
Returns the selected playlist's ID as a string."
  (let* ((playlists (infrasonic-get-playlists))
         (name (completing-read "Playlist: " playlists nil t)))
    (alist-get name playlists nil nil #'equal)))

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

;; TODO: Implement this. I am imagining a completing-read menu for different options similar to the
;;       library one.
(defun listen-subsonic-source ()
  "Present a list of options for Subsonic sources.
Returns a cons (source . list of `listen-track's)."
  (let* ((source (completing-read "Source: "
                                  '("Find"
                                    "Starred Tracks"
                                    "Playlist"
                                    "Search"
                                    "Random")
                                  nil t))
         (tracks (pcase source
                   ("Starred Tracks"
                    (listen-subsonic-get-starred-tracks))
                   ("Find"
                    (listen-subsonic-find))
                   ("Playlist"
                    (listen-subsonic--get-playlist-tracks (infrasonic-read-playlist)))
                   ("Search"
                    (let ((query (read-string "Search: ")))
                      (listen-subsonic-search-tracks query)))
                   ("Random"
                    (listen-subsonic-get-random-tracks 100)))))
    (cons source tracks)))

(defun listen-queue-add-from-subsonic ()
  "Present a list of Subsonic sources, and add tracks from that source to a queue."
  (interactive)
  (let ((tracks (cdr (listen-subsonic-source)))
        (queue (listen-queue-complete :allow-new-p t)))
    (listen-queue-add-tracks tracks queue)))

(defun listen-library-from-subsonic ()
  (interactive)
  (let* ((src (listen-subsonic-source)))
    (listen-library (cdr src)
                    :name (format "Subsonic: %s" (car src)))))

;; TODO: Truncate search results before it hits affixation
(defun listen-subsonic-search (query)
  "Search the server for QUERY, and display artists, albums and tracks.

- Selecting a track adds it to the queue.
- Selecting an artist or album opens the `listen-subsonic-find' browsing functionality."
  (interactive (list (read-string "Search: ")))
  (let* ((items (infrasonic-search query))
         (entries nil)
         (queue ))

    (unless items
      (user-error "No search results for '%s'" query))

    ;; Build entries with unique display names.
    (dolist (item items)
      (let* ((type (alist-get 'subsonic-type item))
             (face (pcase type
                     (:artist 'listen-artist)
                     (:album 'listen-album)
                     (:track 'listen-title)))
             (name (propertize
                    (truncate-string-to-width
                     ;; Ensure that tracks have a name elem
                     (alist-get 'name item)
                     (- listen-subsonic--menu-max-width 5) 0 nil t)
                    'face face))
             (disp-name name)
             (count 1))
        (while (assoc disp-name entries #'equal)
          (cl-incf count)
          (setq disp-name (format "%s %s"
                                  name
                                  (propertize (format "(%d)" count)
                                              'face 'shadow))))
        (push (cons disp-name item) entries)))
    (setq entries (nreverse entries))

    (let* ((affix-fn (listen-subsonic--affixation
                      entries
                      #'listen-subsonic--item-suffix nil
                      #'listen-subsonic--item-prefix nil))
           ;; convert keyword to string for group function
           (group-fn (lambda (cand transform)
                       (if transform
                           cand
                         (let ((type (alist-get 'subsonic-type (alist-get cand entries nil nil #'equal))))
                           (pcase type
                             (:artist "Artists")
                             (:album "Albums")
                             (:track "Songs"))))))
           (selected
            (listen-subsonic--completing-read
             "Select: " entries
             `((affixation-function . ,affix-fn)
               (group-function . ,group-fn)))))

      (let ((type (alist-get 'subsonic-type selected)))
        (pcase type
          (:artist
           (when-let* ((name (alist-get 'name selected))
                       (result (listen-subsonic--find-step
                                :artist
                                (alist-get 'id selected)
                                name))
                       (tracks (funcall (nth 0 result))))
             (listen-queue-add-tracks tracks (listen-queue-complete :allow-new-p t))
             (message "Added %d tracks from '%s'." (length tracks) name)))
          (:album
           (when-let* ((name (alist-get 'name selected))
                       (result (listen-subsonic--find-step
                                :album
                                (alist-get 'id selected)
                                name))
                       (tracks (funcall (nth 0 result))))
             (listen-queue-add-tracks tracks (listen-queue-complete :allow-new-p t))
             (message "Added %d tracks from '%s'." (length tracks) name)))
          (:track
           ;; add a track to the queue
           (let ((track (listen-subsonic--json-to-listen selected)))
             (listen-queue-add-tracks (list track) (listen-queue-complete :allow-new-p t))
             (message "Added '%s' to the queue." (listen-track-title track)))))))))

;; TODO: Make this send a clear cache request to server too?
(defun listen-subsonic-clear-cache ()
  "Delete the Subsonic cache directory and its contents."
  (interactive)
  (when (file-exists-p listen-subsonic-cache-dir)
    (delete-directory listen-subsonic-cache-dir t))
  (message "Cleared Subsonic cache."))

;; Completing read browser
;; TODO: unify the logic used by the minibuffer browser and the buffer browser
;; TODO: have this add to queue
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

(defun listen-subsonic--find-step (level id name &optional history)
  "Recursive browser navigation function for `listen-subsonic-find'.
Returns a list (function name) for the selected action, or nil to go up/back.

LEVEL, ID, and NAME define the current location.
HISTORY is a stack containing the user's navigation history."
  (let* ((items (listen-subsonic--get-nodes level id))
         (next (listen-subsonic--browser-next-level level))
         (prompt (if (eq level :artists)
                     "Library: "
                   (let ((path (mapcar (lambda (h) (nth 2 h)) history)))
                     (format "%s / %s: " (string-join (reverse path) " / ") name))))
         (entries nil))

    ;; ".." and "[All]"
    (when history
      (push (cons (propertize ".." 'face 'shadow) :up) entries))
    (push (cons (propertize "[All]" 'face 'shadow) :this) entries)

    ;; Prepare candidates
    (dolist (item items)
      (let* ((type (alist-get 'subsonic-type item))
             (face (pcase type
                     (:artist 'listen-artist)
                     (:album 'listen-album)
                     (:track 'listen-title)))
             (name (propertize (alist-get 'name item)
                               'face face))
             (disp-name (truncate-string-to-width name
                                                  (- listen-subsonic--menu-max-width 5) 0 nil t))
             (count 1))
        (while (assoc disp-name entries #'equal)
          (cl-incf count)
          (setq disp-name (format "%s (%d)" name count)))
        (push (cons disp-name item) entries)))
    (setq entries (nreverse entries))

    (let* ((affix-fn (listen-subsonic--affixation
                      entries
                      #'listen-subsonic--item-suffix nil
                      #'listen-subsonic--item-prefix nil))
           (selection (listen-subsonic--completing-read
                       prompt entries
                       `((affixation-function . ,affix-fn)
                         (display-sort-function . identity)
                         (cycle-sort-function . identity)))))

      ;; Handle user selection
      (cond
       ;; ".."
       ((eq selection :up)
        (apply #'listen-subsonic--find-step (car history))) ; Latest history
       ;; "[All]"
       ((eq selection :this)
        (list (lambda () (listen-subsonic--get-all-tracks id level))
              (format "Subsonic: %s" name)))
       ;; Folder/artist/album
       ((and (listp selection) (alist-get 'isDir selection))
        (listen-subsonic--find-step next
                                    (alist-get 'id selection)
                                    (alist-get 'name selection)
                                    (cons (list level id name history) history))) ; Add history
       ;; Song
       (t
        (list (lambda () (list (listen-subsonic--json-to-listen selection)))
              (format "Subsonic: %s" (alist-get 'name selection))))))))

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
      (infrasonic-star id star-p
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
  (setq-local revert-buffer-function #'listen-subsonic--dired-revert)
  (defvar-local listen-subsonic--dired-history nil)
  (defvar-local listen-subsonic--dired-current-id nil)
  (defvar-local listen-subsonic--dired-current-name nil)
  (defvar-local listen-subsonic--dired-current-level nil))

;; TODO: make it easier to add to queue
;; TODO: Allow selecting multiple similar to dired (m to mark)
;; TODO: remember point position when going down and back up
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
      (infrasonic-get-art url file
                          (lambda (_)
                            (cl-decf listen-subsonic--art-active)
                            (listen-subsonic--display-art file buf pos)
                            (listen-subsonic--process-art-queue))))))

(defun listen-subsonic--dired-fetch-art (id buf pos)
  "Queue a download for artwork with ID to be displayed at POS in BUF.

If artwork exists in `listen-subsonic-cache-dir', that will be used. Otherwise, art will be
downloaded.
Art is asynchronously displayed in the Listen Subsonic Dired buffer as it is downloaded."
  (unless (file-exists-p listen-subsonic-cache-dir)
    (make-directory listen-subsonic-cache-dir))
  (let ((file (expand-file-name (format "%s.jpg" id) listen-subsonic-cache-dir))
        (url (infrasonic-get-art-url id 64)))
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
    (goto-char (point-min))
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
    (if tracks
        (progn
          (listen-queue-add-tracks tracks (listen-queue-complete))
          (message "Added %d tracks to the queue." (length tracks)))
      (message "No tracks found."))))

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
  (if-let* ((prev (pop listen-subsonic--dired-history)))
      (listen-subsonic--dired-render (nth 0 prev) (nth 1 prev) (nth 2 prev))
    (message "This is the highest level.")))

(defun listen-subsonic--dired-revert (_ignore-auto _noconfirm)
  "Reload the current browser view.

Re-fetches data for the current ID and level from the API."
  (let ((pt (point)))
    (listen-subsonic--dired-render listen-subsonic--dired-current-id
                                   listen-subsonic--dired-current-name
                                   listen-subsonic--dired-current-level)
    (goto-char pt)
    ;; Ensure we are snapped to the button
    (listen-subsonic--dired-next-line 0)))

(provide 'listen-subsonic)
;;; listen-subsonic.el ends here
