;;; listen-subsonic.el                    -*- lexical-binding: t; -*-

(require 'url)
(require 'json)
(require 'auth-source)

(defgroup navidrome nil
  "A minimal from-scratch Navidrome/Subsonic client."
  :group 'applications)

(defcustom navidrome-server-url "music.biglarge.win"
  "The base URL of your Navidrome/Subsonic server.
e.g., \"https://music.example.com\""
  :type 'string
  :group 'navidrome)

(defcustom navidrome-player "mpv"
  "The external command used to play music."
  :type 'string
  :group 'navidrome)

;;;###
;;; Internal Helper Functions
;;;###

(defun navidrome--get-credentials ()
  "Fetch user credentials securely from `auth-source`."
  (let ((auth (auth-source-search :host navidrome-server-url)))
    (when auth
      (car auth))))

(defun navidrome--random-string (length)
  ""
  (let* ((letters "abcdefghijklmnopqrstuvwxyz")
         (let-len (length letters))
         (rand-list (make-list length 0)))
    (setq rand-list
          (mapcar (lambda (_) (aref letters (random let-len))) rand-list))
    (concat rand-list)))

(defun navidrome--build-url (base-url params)
  ""
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

(defun navidrome--get-auth-params ()
  "Return auth info alist for API calls."
  (let* ((creds (navidrome--get-credentials))
         (user (plist-get creds :user))
         (pass (funcall (plist-get creds :secret)))
         (salt (navidrome--random-string 6))
         (token (md5 (concat pass salt))))
    `(("u" . ,user)
      ("t" . ,token)
      ("s" . ,salt)
      ("v" . "1.16.1")
      ("c" . "listen-el")
      ("f" . "json"))))

(defun navidrome--get-stream-url (id)
  "Return a signed url for MPV to play directly."
  (let ((params (append (navidrome--get-auth-params) `(("id" . ,id))))
        (base (concat "https://" navidrome-server-url "/rest/stream.view")))
    (navidrome--build-url base params)))

(defun navidrome-search-tracks (query)
  "Search Navidrome and return a list of `listen-track' objects."
  (require 'listen-lib)
  (let* ((params (append '(("query" . query))))
         (response (navidrome--api-call "search3" `(("query" . ,query) ("songCount" . "50"))))
         (search-result (cdr (assoc 'searchResult3 response)))
         (songs (cdr (assoc 'song search-result))))
    (mapcar (lambda (s)
              (let ((id (cdr (assoc 'id s))))
                ;; filename artist title album number genre (duration 0) date rating etc metadata)
                (make-listen-track
                 :filename (navidrome--get-stream-url id) ; silly mpv
                 :artist (cdr (assoc 'artist s))
                 :title (cdr (assoc 'title s))
                 :album (cdr (assoc 'album s))
                 :number (number-to-string (or (cdr (assoc 'track s)) 0)) ;; this is expected to be a string?
                 :genre (cdr (assoc 'genre s))
                 :duration (or (cdr (assoc 'duration s)) 0)
                 :date (cdr (assoc 'year s))
                 :rating (cdr (assoc 'userRating s))
                 :metadata s
                 :etc `((source . "navidrome")
                        (id . ,id)))))
            songs)))

(defun navidrome--api-call (endpoint &optional params)
  "Make a call to the Subsonic API and return the parsed JSON.
ENDPOINT is the API method, e.g., \"ping\" or \"getAlbumList2\".
PARAMS is an alist of additional parameters."
  (unless navidrome-server-url
    (error "Please set `navidrome-server-url` first"))
  (let* ((creds (navidrome--get-credentials))
         (user (plist-get creds :user))
         (pass (funcall (plist-get creds :secret)))
         (salt (navidrome--random-string 6))
         (token (md5 (concat pass salt)))
         (api-params (append `(("u" . ,user)
                               ("t" . ,token)
                               ("s" . ,salt)
                               ("v" . "1.16.1")
                               ("c" . "navimacs")
                               ("f" . "json"))
                             params))
         (api-url (concat "https://"
                          navidrome-server-url
                          "/rest/"
                          endpoint
                          ".view"))
         (url-request-method "GET")
         (url-request-extra-headers `(("Content-Type" . "application/json")))
         (full-url (navidrome--build-url api-url api-params)))
    (with-current-buffer (url-retrieve-synchronously full-url)
      (goto-char (point-min))
      (when (re-search-forward "\n\n" nil t)
        (let* ((json-string (buffer-substring-no-properties (point) (point-max)))
               (json-data (json-read-from-string json-string))
               (response (cdr (assoc 'subsonic-response json-data))))
          (if (string-equal "ok" (cdr (assoc 'status response)))
              response
            (error "Navidrome API Error: %s" (cdr (assoc 'message (cdr (assoc 'error response)))))))))))

(defun navidrome--play-stream (id)
  "Stream a track with the given ID to an external player."
  (let* ((creds (navidrome--get-credentials))
         (user (plist-get creds :user))
         (pass (funcall (plist-get creds :secret)))
         (salt (navidrome--random-string 6))
         (token (md5 (concat pass salt)))
         (params `(("u" . ,user)
                   ("t" . ,token)
                   ("s" . ,salt)
                   ("v" . "1.16.1")
                   ("c" . "emacs-navidrome")
                   ("id" . ,id)))
         (api-url (concat "https://" navidrome-server-url "/rest/stream.view"))
         (stream-url (navidrome--build-url api-url params)))
    (message "Streaming track ID: %s" id)
    (start-process "navidrome-player" nil navidrome-player stream-url)))

;;;###
;;; User-Facing Interactive Functions
;;;###

(defun navidrome-ping-server ()
  "Ping the server to check connectivity and authentication."
  (interactive)
  (if (navidrome--api-call "ping")
      (message "Successfully pinged Navidrome server!")
    (message "Failed to ping server.")))

(defun navidrome-play-random ()
  "Fetch a list of random songs and play the selected one."
  (interactive)
  (let* ((response (navidrome--api-call "getRandomSongs" '(("size" . "3"))))
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
      (navidrome--play-stream (cdr (assoc 'id chosen-song))))))


(provide 'listen-subsonic)
;;; listen-subsonic.el ends here
