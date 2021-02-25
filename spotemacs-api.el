;;; spotemacs-api.el --- Spotemacs API integration layer  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Daniel Fernandes Martins

;;; Commentary:

;; This library is the interface to the Spotify RESTful API.  It also does some custom handling of
;; the OAuth code exchange via 'simple-httpd

;;; Code:

(require 'simple-httpd)
(require 'request)

;; Due to an issue related to compilation and the way oauth2 uses defadvice
;; (including a FIXME as of 0.1.1), this declaration exists to prevent
;; compiler warnings.
(eval-when-compile
  (defvar url-http-method nil)
  (defvar url-http-data nil)
  (defvar url-http-extra-headers nil)
  (defvar oauth--token-data nil)
  (defvar url-callback-function nil)
  (defvar url-callback-arguments nil)
  (require 'oauth2))

(defcustom spotemacs-oauth2-client-id ""
  "The unique identifier for your application.
More info at https://developer.spotify.com/web-api/tutorial/."
  :group 'spotemacs
  :type 'string)

(defcustom spotemacs-oauth2-client-secret ""
  "The OAuth2 key provided by Spotify.
This is the key that you will need to pass in secure calls to the Spotify
Accounts and Web API services.  More info at
https://developer.spotify.com/web-api/tutorial/."
  :group 'spotemacs
  :type 'string)

(defcustom spotemacs-api-search-limit 50
  "Number of items returned when searching for something using the Spotify API."
  :group 'spotemacs
  :type 'integer)

(defcustom spotemacs-api-locale "en_US"
  "Optional.  The desired language.
This consists of an ISO 639 language code and an ISO 3166-1 alpha-2 country
code, joined by an underscore.  Example: es_MX, meaning Spanish (Mexico).
Provide this parameter if you want the category metadata returned in a
 particular language."
  :group 'spotemacs
  :type 'string)

(defcustom spotemacs-api-country "US"
  "Optional.  An ISO 3166-1 alpha-2 country code.
Provide this parameter if you want to narrow the list of returned categories
to those to a particular country.  If omitted, the returned items will be
globally relevant."
  :group 'spotemacs
  :type 'string)

(defcustom spotemacs-oauth2-callback-port "8080"
  "The port for the httpd to listen on for the OAuth2 callback."
  :group 'spotemacs
  :type 'string)

(defcustom spotemacs-oauth2-callback-endpoint "/spotemacs-callback"
  "The endpoint for the httpd to listen on for the OAuth2 callback."
  :group 'spotemacs
  :type 'string)

(declare-function oauth2-request-access "oauth2")
(declare-function oauth2-refresh-access "oauth2")

(defvar *spotemacs-user*         nil
  "Cached user object.")
(defvar *spotemacs-api-oauth2-token* nil
  "Cached OAuth2 token.")
(defvar *spotemacs-api-oauth2-ts*    nil
  "Unix timestamp in which the OAuth2 token was retrieved.
This is used to manually refresh the token when it's about to expire.")
(defvar *spotemacs-api-oauth2-token-directory* "~/.emacs.d/.cache/spotify"
	"Directory where the OAuth2 token is serialized.")
(defvar *spotemacs-api-oauth2-token-file* (concat *spotemacs-api-oauth2-token-directory* "/" "token")
	"Location where the OAuth2 token is serialized.")

(defconst spotemacs-api-endpoint     "https://api.spotify.com/v1")
(defconst spotemacs-api-oauth2-auth-url  "https://accounts.spotify.com/authorize")
(defconst spotemacs-api-oauth2-token-url "https://accounts.spotify.com/api/token")
(defconst spotemacs-api-oauth2-scopes    "playlist-read-private playlist-read-collaborative playlist-modify-public playlist-modify-private user-read-private user-read-playback-state user-modify-playback-state user-read-playback-state user-read-recently-played")
(defconst spotemacs-api-oauth2-callback  (concat "http://localhost:" spotemacs-oauth2-callback-port "/spotemacs-api-callback"))

(defun spotemacs-api-httpd-stop ()
  "Workaround due to bug in simple-httpd '#httpd-stop."
  (dolist
      (process
       (seq-filter
        (lambda (p)
          (let ((name (process-name p)))
            (or (string-prefix-p "httpd" name) (string-prefix-p "localhost" name))))
        (process-list)))
    (delete-process process)))

(defun spotemacs-api-httpd-process-status ()
  "Answer the process status of the httpd."
  (let ((httpd-process (car (seq-filter
                             (lambda (p)
                               (let ((name (process-name p)))
                                 (string-prefix-p "httpd" name)))
                             (process-list)))))
    (and httpd-process (process-status httpd-process))))

(defun spotemacs-api-start-httpd ()
  "Start the httpd if not already running.  Answer status."
  (let ((is-already-running (spotemacs-api-httpd-process-status)))
    (unless is-already-running
      (setq httpd-port spotemacs-oauth2-callback-port)
      (httpd-start))
    is-already-running))

(defun spotemacs-api-oauth2-request-authorization (auth-url client-id &optional scope state redirect-uri)
  "Request OAuth authorization at AUTH-URL.
Provide SCOPE and STATE to endpoint.  CLIENT-ID is the client id provided by the
provider.  Return the code provided by the service.  Replaces functionality from
built-in OAuth lib by running a local httpd to parse the code instead of asking
the user to paste it in."
  (let ((is-already-running (spotemacs-api-start-httpd))
        (oauth-code nil))
    (defservlet* spotemacs-api-callback text/html (code)
      (setq oauth-code code)
      (insert "<p>Spotemacs is connected. You can return to Emacs</p>
<script type='text/javascript'>setTimeout(function () {close()}, 1500);</script>"))
    (browse-url (concat auth-url
                        (if (string-match-p "\?" auth-url) "&" "?")
                        "client_id=" (url-hexify-string client-id)
                        "&response_type=code"
                        "&redirect_uri=" (url-hexify-string (or redirect-uri "urn:ietf:wg:oauth:2.0:oob"))
                        (if scope (concat "&scope=" (url-hexify-string scope)) "")
                        (if state (concat "&state=" (url-hexify-string state)) "")))
    (let ((retries 0))
      (while (and (not oauth-code)
                  (< retries 5))
        (sleep-for 0 500)
        (setq retries (1+ retries))))
    (unless is-already-running
      (run-at-time 1 nil #'spotemacs-api-httpd-stop))
    oauth-code))

(defun spotemacs-api-oauth2-auth (auth-url token-url client-id client-secret &optional scope state redirect-uri)
  "Authenticate application via OAuth2.
Send CLIENT-ID and CLIENT-SECRET to AUTH-URL.  Get code and send to TOKEN-URL.
Replaces functionality from built-in OAuth lib to call spotemacs-specific
function that runs a local httpd for code -> token exchange."
  (oauth2-request-access
   token-url
   client-id
   client-secret
   (spotemacs-api-oauth2-request-authorization
    auth-url client-id scope state redirect-uri)
		redirect-uri))

(defun spotemacs-api-serialize-token ()
	"Save OAuth2 token to file."
	(unless (file-exists-p *spotemacs-api-oauth2-token-directory*)
		(make-directory *spotemacs-api-oauth2-token-directory* t))
	(and
		(not (null *spotemacs-api-oauth2-token-file*))
		(not (null *spotemacs-api-oauth2-token*))
		(progn
			(delete-file *spotemacs-api-oauth2-token-file*)
			(make-empty-file *spotemacs-api-oauth2-token-file*)
			t)
		(with-temp-file *spotemacs-api-oauth2-token-file*
			(prin1 `(,*spotemacs-api-oauth2-token* ,*spotemacs-api-oauth2-ts*) (current-buffer)))))

(defun spotemacs-api-deserialize-token ()
	"Read OAuth2 token from file."
	(and
		(file-exists-p *spotemacs-api-oauth2-token-file*)
		(with-temp-buffer
			(insert-file-contents *spotemacs-api-oauth2-token-file*)
			(if (= 0 (buffer-size (current-buffer)))
				nil
				(progn
					(goto-char (point-min))
					(pcase-let ((`(,spotemacs-api-oauth2-token ,spotemacs-api-oauth2-ts) (read (current-buffer))))
						(setq *spotemacs-api-oauth2-token* spotemacs-api-oauth2-token)
						(setq *spotemacs-api-oauth2-ts* spotemacs-api-oauth2-ts)))))))

(defun spotemacs-api-persist-token (token now)
	"Persist TOKEN and current time NOW to disk and set in memory too."
  (setq *spotemacs-api-oauth2-token* token)
  (setq *spotemacs-api-oauth2-ts* now)
	(spotemacs-api-serialize-token))

;; Do not rely on the auto-refresh logic from oauth2.el, which seems broken for async requests
(defun spotemacs-api-oauth2-token ()
  "Retrieve the Oauth2 access token used to interact with the Spotify API.
Use the first available token in order of: memory, disk, retrieve from API via
OAuth2 protocol.  Refresh if expired."
  (let ((now (string-to-number (format-time-string "%s"))))
    (if (null (or *spotemacs-api-oauth2-token* (spotemacs-api-deserialize-token)))
      (let ((token (spotemacs-api-oauth2-auth spotemacs-api-oauth2-auth-url
                     spotemacs-api-oauth2-token-url
                     spotemacs-oauth2-client-id
                     spotemacs-oauth2-client-secret
                     spotemacs-api-oauth2-scopes
                     nil
                     spotemacs-api-oauth2-callback)))
				(spotemacs-api-persist-token token now)
        (if (null token)
          (user-error "OAuth2 authentication failed")
          token))
			;; Spotify tokens appear to expire in 3600 seconds (60 min). We renew
			;; at 3000 (50 min) to play it safe
      (if (> now (+ *spotemacs-api-oauth2-ts* 3000))
        (let ((token (oauth2-refresh-access *spotemacs-api-oauth2-token*)))
					(spotemacs-api-persist-token token now)
          (if (null token)
            (user-error "Could not refresh OAuth2 token")
            token))
        *spotemacs-api-oauth2-token*))))

(defun spotemacs-api-call-async (method uri &optional data callback)
  "Make a request to the given Spotify service endpoint URI via METHOD.
Call CALLBACK with the parsed JSON response."
	(request (concat spotemacs-api-endpoint uri)
		:headers `(("Authorization" .
								 ,(format "Bearer %s" (oauth2-token-access-token (spotemacs-api-oauth2-token))))
								("Accept" . "application/json")
								("Content-Type" . "application/json")
								("Content-Length" . ,(length data)))
		:type method
		:parser (lambda ()
							(let ((json-object-type 'hash-table)
										 (json-array-type 'list)
										 (json-key-type 'symbol))
								(when (> (buffer-size) 0)
									(json-read))))
		:encoding 'utf-8
		:data data
		:success (cl-function
							 (lambda (&rest data &key response &allow-other-keys)
								 (when callback (funcall callback (request-response-data response)))))
		:error (cl-function
						 (lambda (&rest args &key error-thrown &allow-other-keys)
							 (message "Got error: %S" error-thrown)))))

(defun spotemacs-api-current-user (callback)
  "Call CALLBACK with the currently logged in user."
  (if *spotemacs-user*
      (funcall callback *spotemacs-user*)
    (spotemacs-api-call-async
     "GET"
     "/me"
     nil
     (lambda (user)
       (setq *spotemacs-user* user)
       (funcall callback user)))))

(defun spotemacs-api-get-items (json)
  "Return the list of items from the given JSON object."
  (gethash 'items json))

(defun spotemacs-api-get-search-track-items (json)
  "Return track items from the given search results JSON object."
  (spotemacs-api-get-items (gethash 'tracks json)))

(defun spotemacs-api-get-search-playlist-items (json)
  "Return playlist items from the given search results JSON object."
  (spotemacs-api-get-items (gethash 'playlists json)))

(defun spotemacs-api-get-message (json)
  "Return the message from the featured playlists JSON object."
  (gethash 'message json))

(defun spotemacs-api-get-playlist-tracks (json)
  "Return the list of tracks from the given playlist JSON object."
  (mapcar #'(lambda (item)
              (gethash 'track item))
          (spotemacs-api-get-items json)))

(defun spotemacs-api-get-track-album (json)
  "Return the simplified album object from the given track JSON object."
  (gethash 'album json))

(defun spotemacs-api-get-track-number (json)
  "Return the track number from the given track JSON object."
  (gethash 'track_number json))

(defun spotemacs-api-get-disc-number (json)
  "Return the disc number from the given track JSON object."
  (gethash 'disc_number json))

(defun spotemacs-api-get-track-duration (json)
  "Return the track duration, in milliseconds, from the given track JSON object."
  (gethash 'duration_ms json))

(defun spotemacs-api-get-track-duration-formatted (json)
  "Return the formatted track duration from the given track JSON object."
  (format-seconds "%m:%02s" (/ (spotemacs-api-get-track-duration json) 1000)))

(defun spotemacs-api-get-track-album-name (json)
  "Return the album name from the given track JSON object."
  (spotemacs-api-get-item-name (spotemacs-api-get-track-album json)))

(defun spotemacs-api-get-track-artist (json)
  "Return the first simplified artist object from the given track JSON object."
  (car (gethash 'artists json)))

(defun spotemacs-api-get-track-artist-name (json)
  "Return the first artist name from the given track JSON object."
  (spotemacs-api-get-item-name (spotemacs-api-get-track-artist json)))

(defun spotemacs-api-get-track-popularity (json)
  "Return the popularity from the given track/album/artist JSON object."
  (gethash 'popularity json))

(defun spotemacs-api-is-track-playable (json)
  "Return whether the given track JSON object is playable by the current user."
  (not (eq :json-false (gethash 'is_playable json))))

(defun spotemacs-api-get-item-name (json)
  "Return the name from the given track/album/artist JSON object."
  (gethash 'name json))

(defun spotemacs-api-get-item-id (json)
  "Return the id from the given JSON object."
  (gethash 'id json))

(defun spotemacs-api-get-item-uri (json)
  "Return the uri from the given track/album/artist JSON object."
  (gethash 'uri json))

(defun spotemacs-api-get-playlist-track-count (json)
  "Return the number of tracks of the given playlist JSON object."
  (gethash 'total (gethash 'tracks json)))

(defun spotemacs-api-get-playlist-owner-id (json)
  "Return the owner id of the given playlist JSON object."
  (spotemacs-api-get-item-id (gethash 'owner json)))

(defun spotemacs-api-search (type query page callback)
  "Search artists, albums, tracks or playlists.
Call CALLBACK with PAGE of items that match QUERY, depending on TYPE."
  (let ((offset (* spotemacs-api-search-limit (1- page))))
    (spotemacs-api-call-async
     "GET"
     (concat "/search?"
             (url-build-query-string `((q      ,query)
                                       (type   ,type)
                                       (limit  ,spotemacs-api-search-limit)
                                       (offset ,offset)
                                       (market from_token))
                                     nil t))
     nil
     callback)))

(defun spotemacs-api-featured-playlists (page callback)
  "Call CALLBACK with the given PAGE of Spotify's featured playlists."
  (let ((offset (* spotemacs-api-search-limit (1- page))))
    (spotemacs-api-call-async
     "GET"
     (concat "/browse/featured-playlists?"
             (url-build-query-string `((locale  ,spotemacs-api-locale)
                                       (country ,spotemacs-api-country)
                                       (limit   ,spotemacs-api-search-limit)
                                       (offset  ,offset))
                                     nil t))
     nil
     callback)))

(defun spotemacs-api-user-playlists (user-id page callback)
  "Call CALLBACK with the PAGE of playlists for the given USER-ID."
  (let ((offset (* spotemacs-api-search-limit (1- page))))
    (spotemacs-api-call-async
     "GET"
     (concat (format "/users/%s/playlists?" (url-hexify-string user-id))
             (url-build-query-string `((limit  ,spotemacs-api-search-limit)
                                       (offset ,offset))
                                     nil t))
     nil
     callback)))

(defun spotemacs-api-playlist-create (user-id name public callback)
  "Create a new playlist with NAME for the given USER-ID.
Make PUBLIC if true.  Call CALLBACK with results"
  (spotemacs-api-call-async
   "POST"
   (format "/users/%s/playlists" (url-hexify-string user-id))
   (format "{\"name\":\"%s\",\"public\":\"%s\"}" name (if public "true" "false"))
   callback))

(defun spotemacs-api-playlist-add-track (user-id playlist-id track-id callback)
  "Add TRACK-ID to PLAYLIST-ID.
Added by USER-ID.  Call CALLBACK with results."
  (spotemacs-api-playlist-add-tracks user-id playlist-id (list track-id) callback))

(defun spotemacs-api-format-id (type id)
  "Format ID.  Wrap with TYPE if necessary."
  (if (string-match-p "spotify" id)
      (format "\"%s\"" id)
    (format "\"spotify:%s:%s\"" type id)))

(defun spotemacs-api-playlist-add-tracks (user-id playlist-id track-ids callback)
  "Add TRACK-IDs to PLAYLIST-ID for USER-ID.
Call CALLBACK with results."
  (let ((tracks (format "%s" (mapconcat (lambda (x) (spotemacs-api-format-id "track" x)) track-ids ","))))
    (spotemacs-api-call-async
     "POST"
     (format "/users/%s/playlists/%s/tracks"
             (url-hexify-string user-id) (url-hexify-string playlist-id))
     (format "{\"uris\": [ %s ]}" tracks)
     callback)))

(defun spotemacs-api-playlist-follow (playlist callback)
  "Add the current user as a follower of PLAYLIST.
Call CALLBACK with results."
  (let ((owner (spotemacs-api-get-playlist-owner-id playlist))
        (id (spotemacs-api-get-item-id playlist)))
    (spotemacs-api-call-async
     "PUT"
     (format "/users/%s/playlists/%s/followers"
             (url-hexify-string owner)
             (url-hexify-string id))
     nil
     callback)))

(defun spotemacs-api-playlist-unfollow (playlist callback)
  "Remove the current user as a follower of PLAYLIST.
Call CALLBACK with results."
  (let ((owner (spotemacs-api-get-playlist-owner-id playlist))
        (id (spotemacs-api-get-item-id playlist)))
    (spotemacs-api-call-async
     "DELETE"
     (format "/users/%s/playlists/%s/followers"
             (url-hexify-string owner)
             (url-hexify-string id))
     nil
     callback)))

(defun spotemacs-api-playlist-tracks (playlist page callback)
  "Call CALLBACK with PAGE of results of tracks from PLAYLIST."
  (let ((owner (spotemacs-api-get-playlist-owner-id playlist))
        (id (spotemacs-api-get-item-id playlist))
        (offset (* spotemacs-api-search-limit (1- page))))
    (spotemacs-api-call-async
     "GET"
     (concat (format "/users/%s/playlists/%s/tracks?"
                     (url-hexify-string owner)
                     (url-hexify-string id))
             (url-build-query-string `((limit  ,spotemacs-api-search-limit)
                                       (offset ,offset)
                                       (market from_token))
                                     nil t))
     nil
     callback)))

(defun spotemacs-api-album-tracks (album page callback)
  "Call CALLBACK with PAGE of tracks for ALBUM."
  (let ((album-id (spotemacs-api-get-item-id album))
        (offset (* spotemacs-api-search-limit (1- page))))
    (spotemacs-api-call-async
     "GET"
     (concat (format "/albums/%s/tracks?"
                     (url-hexify-string album-id))
             (url-build-query-string `((limit ,spotemacs-api-search-limit)
                                       (offset ,offset)
                                       (market from_token))
                                     nil t))
     nil
     callback)))

(defun spotemacs-api-popularity-bar (popularity)
  "Return the popularity indicator bar proportional to POPULARITY.
Parameter must be a number between 0 and 100."
  (let ((num-bars (truncate (/ popularity 10))))
    (concat (make-string num-bars ?X)
            (make-string (- 10 num-bars) ?-))))

(defun spotemacs-api-recently-played (page callback)
  "Call CALLBACK with PAGE of recently played tracks."
  (let ((offset (* spotemacs-api-search-limit (1- page))))
    (spotemacs-api-call-async
     "GET"
     (concat "/me/player/recently-played?"
             (url-build-query-string `((limit  ,spotemacs-api-search-limit)
                                       (offset ,offset))
                                     nil t))
     nil
     callback)))

(defun spotemacs-api-device-list (callback)
  "Call CALLBACK with the list of devices available for use with Spotify Connect."
  (spotemacs-api-call-async
   "GET"
   "/me/player/devices"
   nil
   callback))

(defun spotemacs-api-transfer-player (device-id &optional callback)
  "Transfer playback to DEVICE-ID and determine if it should start playing.
Call CALLBACK with result if provided."
  (spotemacs-api-call-async
   "PUT"
   "/me/player"
   (format "{\"device_ids\":[\"%s\"]}" device-id)
   callback))

(defun spotemacs-api-set-volume (device-id percentage &optional callback)
  "Set the volume level to PERCENTAGE of max for DEVICE-ID."
  (spotemacs-api-call-async
   "PUT"
   (concat "/me/player/volume?"
           (url-build-query-string `((volume_percent ,percentage)
                                     (device_id      ,device-id))
                                   nil t))
   nil
   callback))

(defun spotemacs-api-get-player-status (callback)
  "Call CALLBACK with the Spotify Connect status of the currently active player."
  (spotemacs-api-call-async
   "GET"
   "/me/player"
   nil
   callback))

(defun spotemacs-api-play (&optional callback uri context)
  "Play a track.  If no args, resume playing current track.
Otherwise, play URI in CONTEXT.  Call CALLBACK with results if provided."
  (spotemacs-api-call-async
   "PUT"
   "/me/player/play"
   (concat " { "
           (cond ((and uri context) (format "\"context_uri\": \"%s\", \"offset\": {\"uri\": \"%s\"}" context uri))
                 (context           (format "\"context_uri\": \"%s\"" context))
                 (uri               (format "\"uris\": [ \"%s\" ]" uri))
                 (t                  ""))
           " } ")
   callback))

(defun spotemacs-api-pause (&optional callback)
  "Pause the currently playing track.
Call CALLBACK if provided."
  (spotemacs-api-call-async
   "PUT"
   "/me/player/pause"
   nil
   callback))

(defun spotemacs-api-next (&optional callback)
  "Skip to the next track.
Call CALLBACK if provided."
  (spotemacs-api-call-async
   "POST"
   "/me/player/next"
   nil
   callback))

(defun spotemacs-api-previous (&optional callback)
  "Skip to the previous track.
Call CALLBACK if provided."
  (spotemacs-api-call-async
   "POST"
   "/me/player/previous"
   nil
   callback))

(defun spotemacs-api-repeat (state &optional callback)
  "Set repeat of current track to STATE.
Call CALLBACK if provided."
  (spotemacs-api-call-async
   "PUT"
   (concat "/me/player/repeat?"
           (url-build-query-string `((state ,state))
                                   nil t))
   nil
   callback))

(defun spotemacs-api-shuffle (state &optional callback)
  "Set repeat of current track to STATE.
Call CALLBACK if provided."
  (spotemacs-api-call-async
   "PUT"
   (concat "/me/player/shuffle?"
           (url-build-query-string `((state ,state))
                                   nil t))
   nil
   callback))

(provide 'spotemacs-api)
;;; spotemacs-api.el ends here
