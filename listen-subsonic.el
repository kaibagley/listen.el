;;; listen-subsonic.el                    -*- lexical-binding: t; -*-

;; TODO: Add listen-subsonic-queue-from-playlist
;; TODO: Some kind of indicator to show if track is starred or not
(require 'url)
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

(defface listen-starred
  '((t :inherit font-lock-warning-face :foreground))
  "Face for starred Subsonic tracks."
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
     :etc `((source . "navidrome")
            (id . ,id)
            (starred . ,(if starred t nil))))))

(defun listen-subsonic--get-tracks (endpoint key &optional params)
  "Fetch tracks from ENDPOINT.
PARAMS are optional API parameters."
  (let* ((response (listen-subsonic--api-call endpoint params))
         (data (alist-get key response))
         (songs (alist-get 'song data)))
    (mapcar #'listen-subsonic--json-to-listen songs)))

;; TODO: This blocks emacs while waiting for response
;; Look into consult's async features at some
;; stage? Maybe not necessary but will allow searching way more than 50
(defun listen-subsonic-search-tracks (query)
  "Return a list of `listen-track' objects.
Uses the Subsonic API's \"search3\" endpoint with QUERY as the search query.
The maximum returned tracks is 50."
  (listen-subsonic--get-tracks "search3" 'searchResult3
                               `(("query" . ,query) ("songCount" . "50"))))

(defun listen-subsonic-get-starred-tracks ()
  "Fetch all starred songs from Navidrome."
  (listen-subsonic--get-tracks "getStarred" 'starred))

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
              ((equal (map-elt (listen-track-etc track) 'source) "navidrome"))
              (id (alist-get 'id (listen-track-etc track))))
    (let* ((params `(("id". ,id)
                     ("submission" . ,(if submission-p "true" "false")))))
      (listen-subsonic--api-call "scrobble" params #'ignore))))

(defun listen-subsonic-track-now-playing (player)
  "Notifies the Subsonic server that we have started playing a track.
Should be added to `listen-track-start-functions'."
  (listen-subsonic--scrobble player nil))

(defun listen-subsonic-track-finished (player)
  "Notifies the Subsonic server that we have finished a track.
Should be added to `listen-track-end-functions'."
  (listen-subsonic--scrobble player t))

(defun listen-subsonic--process-api-response ()
  "Parse JSON response from a Subsonic API request.
Returns the response's data, or signals an error.
Should be called from a buffer containing an API response."
  (goto-char (point-min))
  (if (not (re-search-forward "\n\n" nil t))
      (error "Subsonic API response is empty")
    (let ((json-data))
      (decode-coding-region (point) (point-max) 'utf-8)
      (setq json-data (json-parse-buffer :object-type 'alist
                                         :null-object nil
                                         :false-object nil))
      (let ((response (alist-get 'subsonic-response json-data)))
        (if (string-equal "ok" (alist-get 'status response))
            response
          (error "Subsonic API response returned error: %s"
                 (alist-get 'message (alist-get 'error response))))))))

(defun listen-subsonic--api-call (endpoint &optional params callback)
  "Make a call to the Subsonic API.
ENDPOINT is the API method defined by the Subsonic or OpenSubsonic API specifications.
PARAMS is an alist of additional parameters.
If CALLBACK is nil, run synchronously and return the parsed JSON.
If CALLBACK is non-nil, run asynchronously and call CALLBACK with the data.
This function handles error responses, CALLBACK should assume a successful API request."
  (unless listen-subsonic-url
    (user-error "Please set `listen-subsonic-url'."))
  (let* ((api-params (append (listen-subsonic--get-auth-params) params))
         (api-url (listen-subsonic--build-url endpoint api-params)))
    (if callback
        ;; Async request
        (url-retrieve api-url
                      (lambda (status)
                        (let ((err (plist-get status :error)))
                          (if err
                              (message "Subsonic API call error: HTTP %s" err)
                            (funcall callback (listen-subsonic--process-api-response))
                            (kill-buffer (current-buffer)))) ; Creates a new buffer each time??
                      nil t))
      ;; Sync request
      (let ((buf (url-retrieve-synchronously api-url)))
        (unwind-protect
            (with-current-buffer buf (listen-subsonic--process-api-response))
          (kill-buffer buf))))))

;;;###
;;; User-Facing Interactive Functions
;;;###

(defun listen-subsonic-ping-server ()
  "Ping the server to check connectivity and authentication."
  (interactive)
  (if (listen-subsonic--api-call "ping")
      (message "Successfully pinged Navidrome server!")
    (message "Failed to ping server.")))

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
