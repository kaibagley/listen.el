;;; listen-subsonic.el                    -*- lexical-binding: t; -*-

;; TODO: Look at using plz.el for http requests
(require 'url)
(require 'json)
(require 'auth-source)

(defgroup listen-subsonic nil
  "Navidrome/Subsonic options."
  :group 'listen)

(defcustom listen-subsonic-url "music.biglarge.win"
  "The base URL of your Navidrome/Subsonic server.
e.g., \"https://music.example.com\""
  :type 'string
  :group 'listen-subsonic)

;;;###
;;; Internal Helper Functions
;;;###

(defun listen-subsonic--get-credentials ()
  "Fetch user credentials securely from `auth-source`."
  (let ((auth (auth-source-search :host listen-subsonic-url)))
    (when auth
      (car auth))))

(defun listen-subsonic--random-string (length)
  "Generates a random string, for use as a token in a Subsonic request."
  (let* ((letters "abcdefghijklmnopqrstuvwxyz")
         (let-len (length letters))
         (rand-list (make-list length 0)))
    (setq rand-list
          (mapcar (lambda (_) (aref letters (random let-len))) rand-list))
    (concat rand-list)))

(defun listen-subsonic--build-url (base-url params)
  "Build a URL from BASE-URL and PARAMS, to be used as an API call to
Subsonic."
  (if (null params)
      base-url
    (concat base-url
            "?"
            (mapconcat
             (lambda (param)
               (concat (url-hexify-string (car param))
                       "="
                       (url-hexify-string (cdr param))))
             params
             "&"))))

(defun listen-subsonic--get-auth-params ()
  "Return auth info alist for API calls."
  (let* ((creds (listen-subsonic--get-credentials))
         (user (plist-get creds :user))
         (pass (funcall (plist-get creds :secret)))
         (salt (listen-subsonic--random-string 6))
         (token (md5 (concat pass salt))))
    `(("u" . ,user)
      ("t" . ,token)
      ("s" . ,salt)
      ("v" . "1.16.1")
      ("c" . "listen.el")
      ("f" . "json"))))

(defun listen-subsonic--get-stream-url (id)
  "Return a signed url for MPV to play directly."
  (let ((params (append (listen-subsonic--get-auth-params) `(("id" . ,id))))
        (base (concat "https://" listen-subsonic-url "/rest/stream.view")))
    (listen-subsonic--build-url base params)))

(defun listen-subsonic--json-to-listen (s)
  "Convert JSON alist into a listen.el `listen-track' structure."
  (let ((id (cdr (assoc 'id s))))
    (make-listen-track
     :filename (listen-subsonic--get-stream-url id) ; silly mpv
     :artist (cdr (assoc 'artist s))
     :title (cdr (assoc 'title s))
     :album (cdr (assoc 'album s))
     :number (number-to-string (or (cdr (assoc 'track s)) 0))
     :genre (cdr (assoc 'genre s))
     :duration (or (cdr (assoc 'duration s)) 0)
     :date (cdr (assoc 'year s))
     :rating (cdr (assoc 'userRating s))
     ;; every tag should get dumped into the metadata (i think)
     :metadata '((source . "navidrome"))
     :etc `((source . "navidrome")
            (id . ,id)))))

(defun listen-subsonic-search-tracks (query)
  "Search Navidrome and return a list of `listen-track' objects."
  (let* ((response (listen-subsonic--api-call "search3" `(("query" . ,query) ("songCount" . "50"))))
         (search-result (cdr (assoc 'searchResult3 response)))
         (songs (cdr (assoc 'song search-result))))
    (mapcar #'listen-subsonic--json-to-listen songs)))

(defun listen-subsonic-get-starred-tracks ()
  "Fetch all starred songs from Navidrome."
  (let* ((response (listen-subsonic--api-call "getStarred"))
         (starred-result (cdr (assoc 'starred response)))
         (songs (cdr (assoc 'song starred-result))))
    (mapcar #'listen-subsonic--json-to-listen songs)))

(defun listen-subsonic--api-call (endpoint &optional params)
  "Make a call to the Subsonic API and return the parsed JSON.
ENDPOINT is the API method, e.g., \"ping\" or \"getAlbumList2\".
PARAMS is an alist of additional parameters."
  (unless listen-subsonic-url
    (error "Please set `listen-subsonic-url' first"))
  (let* ((creds (listen-subsonic--get-credentials))
         (user (plist-get creds :user))
         (pass (funcall (plist-get creds :secret)))
         (salt (listen-subsonic--random-string 6))
         (token (md5 (concat pass salt)))
         (api-params (append `(("u" . ,user)
                               ("t" . ,token)
                               ("s" . ,salt)
                               ("v" . "1.16.1")
                               ("c" . "listen.el")
                               ("f" . "json"))
                             params))
         (api-url (concat "https://"
                          listen-subsonic-url
                          "/rest/"
                          endpoint
                          ".view"))
         (url-request-method "GET")
         (url-request-extra-headers `(("Content-Type" . "application/json")))
         (full-url (listen-subsonic--build-url api-url api-params)))
    (with-current-buffer (url-retrieve-synchronously full-url)
      (goto-char (point-min))
      (when (re-search-forward "\n\n" nil t)
        (let* ((json-string (decode-coding-string
                             (buffer-substring-no-properties (point) (point-max))
                             'utf-8))
               (json-data (json-read-from-string json-string))
               (response (cdr (assoc 'subsonic-response json-data))))
          (if (string-equal "ok" (cdr (assoc 'status response)))
              response
            (error "Navidrome API Error: %s" (cdr (assoc 'message (cdr (assoc 'error response)))))))))))

;;;###
;;; User-Facing Interactive Functions
;;;###

(defun listen-subsonic-ping-server ()
  "Ping the server to check connectivity and authentication."
  (interactive)
  (if (listen-subsonic--api-call "ping")
      (message "Successfully pinged Navidrome server!")
    (message "Failed to ping server.")))

(defun listen-subsonic-play-random ()
  "Fetch a list of random songs and play the selected one."
  (interactive)
  (let* ((response (listen-subsonic--api-call "getRandomSongs" '(("size" . "3"))))
         (songs (cdr (assoc 'song (cdr (assoc 'randomSongs response)))))
         (song-alist (mapcar (lambda (s)
                                 (cons (format "%s - %s"
                                               (cdr (assoc 'artist s))
                                               (cdr (assoc 'title s)))
                                       s))
                             songs))
         (selection (completing-read "Play song: "
                                     (mapcar #'car song-alist)
                                     nil t))
         (chosen-song (cdr (assoc-string selection song-alist t))))
    (when chosen-song
      (listen-subsonic--play-stream (cdr (assoc 'id chosen-song))))))

;; FIXME: Seems to add all tracks, not just starred...
(defun listen-queue-add-starred-from-subsonic (queue)
  "Add all starred songs from Navidrome to QUEUE."
  (interactive (list
                (progn
                  (require 'listen-queue)
                  (listen-queue-complete :allow-new-p t))))
  (let ((tracks (listen-subsonic-get-starred-tracks)))
    (if tracks
        (progn
          (listen-queue-add-tracks tracks queue)
          (message "Added %d tracks to queue '%s'."
                   (length tracks) (listen-queue-name queue))
          (listen-queue queue))
      (message "No starred songs found."))))

;; TODO: C-u adds to start of queue/next?
(defun listen-queue-add-from-subsonic (query queue)
  "Search Navidrome for QUERY and add results to the current queue."
  (interactive
   (let ((query (read-string "Search Navidrome: ")))
     (list query
           (progn
             (require 'listen-queue)
             (listen-queue-complete :allow-new-p t)))))
  (let* ((tracks (listen-subsonic-search-tracks query))
         (candidates (mapcar
                      (lambda (track)
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
         (selected-tracks (mapcar
                           (lambda (name) (cdr (assoc name candidates)))
                           selected-names)))
    (if selected-tracks
        (progn
          (listen-queue-add-tracks selected-tracks queue)
          (message "Added %d tracks from Navidrome to queue '%s'."
                   (length tracks)
                   (listen-queue-name queue))
          (listen-queue queue))
      (message "No tracks found or added to '%s'" query))))

(defun listen-library-from-subsonic (source)
  "Show a library view for subsonic."
  (interactive
   (list (completing-read "Source: "
                          '("Starred" "Search") nil t)))
  (let ((tracks-fn
         (pcase source
           ("Starred"
            (lambda () (listen-subsonic-get-starred-tracks)))
           ("Search"
            (let ((query (read-string "Search: ")))
              (lambda () (listen-subsonic-search-tracks query)))))))
    (listen-library tracks-fn
                    :name (format "Subsonic: %s" source))))

(provide 'listen-subsonic)
;;; listen-subsonic.el ends here
