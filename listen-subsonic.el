;;; listen-subsonic.el                    -*- lexical-binding: t; -*-

;; TODO: Look at using plz.el for http requests
(require 'url)
(require 'json)
(require 'auth-source)
(require 'listen-queue)

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
  "Generates a random string, for use as a token in a Subsonic API request."
  (let* ((letters "abcdefghijklmnopqrstuvwxyz")
         (let-len (length letters))
         (rand-list (make-list length 0)))
    (setq rand-list
          (mapcar (lambda (_) (aref letters (random let-len))) rand-list))
    (concat rand-list)))

;; TODO: Maybe allow insecure http later?
(defun listen-subsonic--build-url (endpoint params)
  "Build a URL from ENDPOINT and PARAMS, to be used as an API call to
Subsonic."
  (let* ((param-list (mapcar (lambda (p)
                               (list (car p) (url-hexify-string (cdr p))))
                             params))
         (param-str (url-build-query-string param-list nil t)))
    (format "https://%s/rest/%s.view?%s"
            listen-subsonic-url endpoint param-str)))

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
  "Return a URL for MPV to stream from directly.
Includes token, salt, and username retrieved from `auth-source' as
parameters."
  (listen-subsonic--build-url
   "stream"
   (append (listen-subsonic--get-auth-params) `(("id" . ,id)))))

(defun listen-subsonic--json-to-listen (s)
  "Convert JSON alist S into a `listen-track' structure."
  (let ((id (alist-get 'id s)))
    (make-listen-track
     :filename (listen-subsonic--get-stream-url id) ; silly mpv
     :artist (alist-get 'artist s)
     :title (alist-get 'title s)
     :album (alist-get 'album s)
     :number (number-to-string (or (alist-get 'track s) 0))
     :genre (alist-get 'genre s)
     :duration (or (alist-get 'duration s) 0)
     :date (alist-get 'year s)
     :rating (alist-get 'userRating s)
     ;; TODO: Pass all tags we can get to metadata
     :metadata '((source . "navidrome"))
     :etc `((source . "navidrome")
            (id . ,id)))))

(defun listen-subsonic--get-tracks (endpoint key &optional params)
  "Fetch tracks from ENDPOINT.
PARAMS are optional API parameters."
  (let* ((response (listen-subsonic--api-call endpoint params))
         (data (alist-get key response))
         (songs (alist-get 'song data)))
    (mapcar #'listen-subsonic--json-to-listen songs)))

(defun listen-subsonic-search-tracks (query)
  "Return a list of `listen-track' objects."
  (listen-subsonic--get-tracks "search3" 'searchResult3
                               `(("query" . ,query) ("songCount" . "50"))))

(defun listen-subsonic-get-starred-tracks ()
  "Fetch all starred songs from Navidrome."
  (listen-subsonic--get-tracks "getStarred" 'starred))

(defun listen-subsonic--api-call (endpoint &optional params)
  "Make a call to the Subsonic API and return the parsed JSON.
ENDPOINT is the API method, e.g., \"ping\" or \"getAlbumList2\".
PARAMS is an alist of additional parameters."
  (unless listen-subsonic-url
    (error "Please set `listen-subsonic-url'."))
  (let* ((api-params (append (listen-subsonic--get-auth-params) params))
         (api-url (listen-subsonic--build-url endpoint api-params)))
    ;; Maybe make this asynchronous using `url-retrieve' with callback instead?
    (with-current-buffer (url-retrieve-synchronously api-url)
      (goto-char (point-min))
      (when (re-search-forward "\n\n" nil t)
        (let* ((json-data (json-read-from-string
                           (decode-coding-string
                            (buffer-substring-no-properties (point) (point-max))
                            'utf-8)))
               (response (alist-get 'subsonic-response json-data)))
          (if (string-equal "ok" (alist-get 'status response))
              response
            (error "Navidrome API Error: %s"
                   (alist-get 'message (alist-get 'error response)))))))))

;;;###
;;; User-Facing Interactive Functions
;;;###

(defun listen-subsonic-ping-server ()
  "Ping the server to check connectivity and authentication."
  (interactive)
  (if (listen-subsonic--api-call "ping")
      (message "Successfully pinged Navidrome server!")
    (message "Failed to ping server.")))

;; TODO: Make this actually work
(defun listen-subsonic-queue-random (n queue)
  "Fetch and queue a list of N random songs."
  (interactive
   (list
    (read-number "Number of songs: " 10)
    (listen-queue-complete :allow-new-p t)))
  (let* ((tracks (listen-subsonic--get-tracks
                  "getRandomSongs"
                  'randomSongs `(("size" . ,(number-to-string n))))))
    (if tracks
        (progn
          (listen-queue-add-tracks tracks (listen-queue))
          (message "Added %d random tracks to queue '%s'."
                   (length tracks) (listen-queue-name queue))
          (listen-queue queue))
      (message "No tracks returned from server."))))

;; TODO: Decide if these should be here or in listen-queue.el
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
;; TODO; Use annotate-function to make this (and other functions) look better
(defun listen-queue-add-from-subsonic (query queue)
  "Search Navidrome for QUERY and add results to the current queue."
  (interactive
   (list
    (read-string "Search Navidrome: ")
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
          (message "Added %d tracks from Navidrome to queue '%s'."
                   (length selected-tracks)
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
