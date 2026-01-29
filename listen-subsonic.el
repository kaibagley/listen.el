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

;; Declares

(declare-function listen-library "listen-library")

(declare-function listen-subsonic--build-client "listen-subsonic")
(declare-function listen-subsonic--custom-set "listen-subsonic")

;;;; Customisation

(defgroup listen-subsonic nil
  "`listen' options for Subsonic backend."
  :group 'listen)

(defcustom listen-subsonic-url nil
  "The fully-qualified domain name of your Subsonic-compatible server.
For example, \"music.example.com\" or \"192.168.0.0:4533\".
Don't include the procol/scheme or the resource path."
  :type 'string
  :group 'listen-subsonic
  :set #'listen-subsonic--custom-set)

(defcustom listen-subsonic-protocol "https"
  "Protocol to use for calls to Subsonic API.
Must be either \"http\" or \"https\" (default)."
  :type '(choice (const :tag "HTTPS" "https")
                 (const :tag "HTTP" "http"))
  :group 'listen-subsonic
  :set #'listen-subsonic--custom-set)

(defcustom listen-subsonic-api-version "1.16.1"
  "OpenSubsonic API version string to advertise (e.g. \"1.16.1\")."
  :type 'string
  :group 'listen-subsonic
  :set #'listen-subsonic--custom-set)

(defcustom listen-subsonic-timeout 300
  "Request timeout in seconds passed to `plz'."
  :type 'integer
  :group 'listen-subsonic
  :set #'listen-subsonic--custom-set)

(defcustom listen-subsonic-queue-limit 5
  "Max concurrent downloads for `infrasonic''s `plz' queue."
  :type 'integer
  :group 'listen-subsonic
  :set #'listen-subsonic--custom-set)

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

;;;; General helpers

(defun listen-subsonic--custom-set (symbol value)
  "Rebuild the `infrasonic' client with SYMBOL set to VALUE."
  (set-default symbol value)
  (listen-subsonic--build-client))

(defun listen-subsonic--client ()
  "Return the current `infrasonic' client, or build a new one and return that."
  (or listen-subsonic--client
      (progn
        (listen-subsonic--build-client)
        (or listen-subsonic--client
            (user-error "Please set `listen-subsonic-url'.")))))

(defun listen-subsonic--build-client ()
  "Build or rebuild our `listen-subsonic--client' from `listen' user options."
  (setq listen-subsonic--client
        (when (and (stringp listen-subsonic-url)
                   (not (string-empty-p listen-subsonic-url)))
          (infrasonic-make-client
           :url listen-subsonic-url
           :protocol listen-subsonic-protocol
           :user-agent "listen.el"
           :api-version listen-subsonic-api-version
           :queue-limit listen-subsonic-queue-limit
           :timeout listen-subsonic-timeout
           :art-size 128
           :search-max-results listen-subsonic-search-max-results))))

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
       :number (number-to-string (or track 0))
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

;; (defun listen-subsonic-search-tracks (query)
;;   "Search the server for tracks matching QUERY.
;; Returns a list of `listen-track's."
;;   (mapcar #'listen-subsonic--json-to-listen
;;           (infrasonic-search-tracks query)))

(defun listen-subsonic-get-starred-tracks ()
  "Fetch all starred songs from the server.
Returns a list of `listen-track's."
  (mapcar #'listen-subsonic--json-to-listen
          (infrasonic-get-starred-tracks (listen-subsonic--client))))

(defun listen-subsonic--get-playlist-tracks (id)
  "Fetch all tracks in playlist with ID.
Returns a list of `listen-track's."
  (mapcar #'listen-subsonic--json-to-listen
          (infrasonic-get-playlist-tracks (listen-subsonic--client) id)))

(defun listen-subsonic--get-all-tracks (id level)
  "Fetch all tracks under item associated with ID.
Returns a list of `listen-track's.

LEVEL determines what level of the hierarchy we are on:
- :artist: fetches all albums, then all songs by that artist.
- :album: fetches all songs on the album."
  (mapcar #'listen-subsonic--json-to-listen
          (infrasonic-get-all-tracks (listen-subsonic--client) id level)))

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

(defun listen-subsonic--scrobble (player status)
  "Scrobble the STATUS of the current track playing in PLAYER's queue to
the Subsonic API.
Returns the unparsed API response.

Only tracks with the source \"subsonic\" will be scrobbled.
STATUS may be either `:playing' or `:finished'."
  (when-let* ((queue (map-elt (listen-player-etc player) :queue))
              (track (listen-queue-current queue))
              (source (equal (map-elt (listen-track-etc track) 'source) "subsonic"))
              (id (alist-get 'id (listen-track-etc track))))
    (infrasonic-scrobble (listen-subsonic--client) id status)))

(defun listen-subsonic-scrobble-start (player)
  "Notifies the server that we have started playing a track in PLAYER.
Should be added to `listen-track-start-functions'."
  (listen-subsonic--scrobble player :playing))

(defun listen-subsonic-scrobble-end (player)
  "Notifies the server that we have finished a track in PLAYER.
Should be added to `listen-track-end-functions'."
  (listen-subsonic--scrobble player :finished))

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
          (infrasonic-get-random-songs (listen-subsonic--client) n)))

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
  (let* ((playlists (infrasonic-get-playlists (listen-subsonic--client)))
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
  (listen-queue-add-tracks (listen-subsonic-get-starred-tracks)
                           queue))

(defun listen-library-from-subsonic ()
  "Turn a list of `listen-track's into a `listen-library' view."
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
  (let* ((items (infrasonic-search (listen-subsonic--client) query))
         (entries nil))

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

(provide 'listen-subsonic)

;;; listen-subsonic.el ends here
