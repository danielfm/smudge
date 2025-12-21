;;; smudge-api.el --- Smudge API integration layer  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2025 Daniel Martins

;; SPDX-License-Identifier:  GPL-3.0-or-later

;;; Commentary:

;; This library is the interface to the Spotify RESTful API.  It also does some
;; custom handling of the OAuth code exchange via 'simple-httpd

;;; Code:

(require 'simple-httpd)
(require 'request)
(require 'oauth2)
(require 'browse-url)
(require 'plstore)

(defcustom smudge-oauth2-client-id ""
  "The unique identifier for your application.
More info at https://developer.spotify.com/web-api/tutorial/."
  :group 'smudge
  :type 'string)

(defcustom smudge-oauth2-client-secret ""
  "The OAuth2 key provided by Spotify.
This is the key that you will need to pass in secure calls to the Spotify
Accounts and Web API services.  More info at
https://developer.spotify.com/web-api/tutorial/."
  :group 'smudge
  :type 'string)

(defcustom smudge-api-search-limit 50
  "Number of items returned when searching for something using the Spotify API."
  :group 'smudge
  :type 'integer)

(defcustom smudge-api-locale "en_US"
  "Optional.  The desired language.
This consists of an ISO 639 language code and an ISO 3166-1 alpha-2 country
code, joined by an underscore.  Example: es_MX, meaning Spanish (Mexico).
Provide this parameter if you want the category metadata returned in a
 particular language."
  :group 'smudge
  :type 'string)

(defcustom smudge-api-country "US"
  "Optional.  An ISO 3166-1 alpha-2 country code.
Provide this parameter if you want to narrow the list of returned categories
to those to a particular country.  If omitted, the returned items will be
globally relevant."
  :group 'smudge
  :type 'string)

(defcustom smudge-oauth2-callback-scheme "http"
  "The scheme for the httpd to listen on for the OAuth2 callback."
  :group 'smudge
  :type 'string)

(defcustom smudge-oauth2-callback-host "127.0.0.1"
  "The host for the httpd to listen on for the OAuth2 callback."
  :group 'smudge
  :type 'string)

(defcustom smudge-oauth2-callback-port "8080"
  "The port for the httpd to listen on for the OAuth2 callback."
  :group 'smudge
  :type 'string)

(defcustom smudge-oauth2-callback-endpoint "smudge_api_callback"
  "The endpoint for the httpd to listen on for the OAuth2 callback.
Note: This must match the httpd endpoint in `smudge-api-oauth2-request-authorization'."
  :group 'smudge
  :type 'string)

(defvar smudge-user nil
  "Cached user object.")

(defvar smudge-api-oauth2-token nil
  "Cached OAuth2 token.")

(defvar smudge-api-oauth2-auth-code nil
  "Temporary storage for OAuth2 authorization code received from callback.")

(defvar smudge-api-oauth2-callback-state nil
  "Temporary storage for OAuth2 state parameter to validate callback.")

(defvar smudge-api-oauth2-auth-in-progress nil
  "Flag indicating whether OAuth2 authentication is currently in progress.")

(defconst smudge-api-endpoint
  "https://api.spotify.com/v1"
  "Spotify API endpoint.")

(defconst smudge-api-oauth2-auth-url
  "https://accounts.spotify.com/authorize"
  "Spotify API authorization endpoint.")

(defconst smudge-api-oauth2-token-url
  "https://accounts.spotify.com/api/token"
  "Spotify API token endpoint.")

(defconst smudge-api-oauth2-scopes
  (string-join
   '("playlist-read-private"
     "playlist-read-collaborative"
     "playlist-modify-public"
     "playlist-modify-private"
     "user-read-private"
     "user-read-playback-state"
     "user-modify-playback-state"
     "user-read-playback-state"
     "user-read-recently-played"
     "user-library-read"
     "user-library-modify")
   " ")
  "Spotify API scopes required by Smudge.")

(defun smudge-api-oauth2-generate-state ()
  "Generate a random state string for OAuth2 CSRF protection."
  (format "%04x%04x-%04x-%04x-%04x-%04x%04x%04x"
          (random 65536) (random 65536)
          (random 65536)
          (random 65536)
          (random 65536)
          (random 65536) (random 65536) (random 65536)))

(defun smudge-api-oauth2-start-server ()
  "Start the HTTP server for OAuth2 callback.
Uses the port and host configured in `smudge-oauth2-callback-port' and
`smudge-oauth2-callback-host'."
  (let ((port (string-to-number smudge-oauth2-callback-port)))
    (setq httpd-port port)
    (setq httpd-host smudge-oauth2-callback-host)
    (httpd-start)))

(defun smudge-api-oauth2-stop-server ()
  "Stop the HTTP server for OAuth2 callback."
  ;; Kill any remaining httpd connections
  (dolist (proc (process-list))
    (when (and (process-name proc)
               (string-prefix-p "httpd" (process-name proc)))
      (delete-process proc)))
  (httpd-stop))

(defservlet* smudge_api_callback text/html (code state error)
  "Handle OAuth2 callback from Spotify authorization.
Captures the authorization CODE and STATE parameters."
  (cond
   (error
    (insert (format "<html><body><h1>Authorization Error</h1><p>%s</p><p>You can close this window.</p></body></html>" error)))
   ((not (string= state smudge-api-oauth2-callback-state))
    (insert "<html><body><h1>Authorization Error</h1><p>Invalid state parameter.</p><p>You can close this window.</p></body></html>"))
   (t
    (setq smudge-api-oauth2-auth-code code)
    (insert "<html>
<head>
  <title>Authorization Successful</title>
</head>
<body>
  <h1>Authorization Successful!</h1>
  <p>You can return to Emacs.</p>
  <p>This window will close in <span id=\"timer\">5</span> seconds...</p>
  <p><button onclick=\"window.close()\">Close Now</button></p>
  <script>
    let seconds = 5;
    const timerElement = document.getElementById('timer');
    const countdown = setInterval(() => {
      seconds--;
      timerElement.textContent = seconds;
      if (seconds <= 0) {
        clearInterval(countdown);
        window.close();
      }
    }, 1000);
  </script>
</body>
</html>"))))

(defun smudge-api-oauth2-auth (auth-url token-url client-id client-secret &optional scope redirect-uri)
  "Authenticate application via OAuth2.
Send CLIENT-ID and CLIENT-SECRET to AUTH-URL.  Get code and send to TOKEN-URL.
Starts a local HTTP server to capture the authorization code from the callback."
  (let ((inhibit-message t)
        (state (smudge-api-oauth2-generate-state)))
    ;; Set flag to prevent concurrent auth flows
    (setq smudge-api-oauth2-auth-in-progress t)

    ;; Reset auth code from any previous attempt
    (setq smudge-api-oauth2-auth-code nil)
    (setq smudge-api-oauth2-callback-state state)

    ;; Start the HTTP server to listen for the callback
    (smudge-api-oauth2-start-server)

    ;; Build authorization URL and open in browser
    (let ((auth-request-url
           (concat auth-url
                   (if (string-match-p "\\?" auth-url) "&" "?")
                   (url-build-query-string
                    `((client_id ,client-id)
                      (response_type "code")
                      (redirect_uri ,redirect-uri)
                      (scope ,scope)
                      (state ,state))))))
      (browse-url auth-request-url))

    ;; Wait for the authorization code to be received by the callback
    (message "Waiting for authorization callback...")
    (while (not smudge-api-oauth2-auth-code)
      (sleep-for 0.5))

    ;; Exchange the authorization code for an access token using built-in oauth2 function
    (let ((token (oauth2-request-access
                  auth-url
                  token-url
                  client-id
                  client-secret
                  smudge-api-oauth2-auth-code
                  redirect-uri)))

      ;; Stop the HTTP server
      (smudge-api-oauth2-stop-server)

      ;; Store the token using built-in plstore function
      (let ((plstore-id (oauth2-compute-id auth-url token-url scope client-id ""))
            (plstore (plstore-open oauth2-token-file)))
        (setf (oauth2-token-plstore-id token) plstore-id)
        (oauth2--update-plstore plstore token)
        (plstore-close plstore))

      ;; Clean up temporary variables
      (setq smudge-api-oauth2-auth-code nil)
      (setq smudge-api-oauth2-callback-state nil)
      (setq smudge-api-oauth2-auth-in-progress nil)

      token)))

(defun smudge-api-oauth2-load-token ()
  "Load OAuth2 token from disk if it exists."
  (let* ((plstore-id (oauth2-compute-id
                      smudge-api-oauth2-auth-url
                      smudge-api-oauth2-token-url
                      smudge-api-oauth2-scopes
                      smudge-oauth2-client-id
                      ""))
         (plstore (plstore-open oauth2-token-file))
         (stored-data (plstore-get plstore plstore-id)))
    (plstore-close plstore)
    (when stored-data
      (let* ((plist (cdr stored-data))
             (token-data (plist-get plist :access-response))
             (access-token (cdr (assoc 'access_token token-data)))
             (refresh-token (cdr (assoc 'refresh_token token-data))))
        (when access-token
          (make-oauth2-token
           :plstore-id plstore-id
           :client-id smudge-oauth2-client-id
           :client-secret smudge-oauth2-client-secret
           :access-token access-token
           :refresh-token refresh-token
           :token-url smudge-api-oauth2-token-url
           :access-response token-data))))))

(defun smudge-api-oauth2-token ()
  "Retrieve the Oauth2 access token used to interact with the Spotify API.
Use the first available token in order of: memory, disk, retrieve from API via
OAuth2 protocol.  Refresh if expired.  Spin and wait if already in the process
of fetching via another call to this method."
  (let ((inhibit-message t))
    ;; If auth is already in progress, wait for it to complete
    (while smudge-api-oauth2-auth-in-progress
      (sleep-for 0.5))

    ;; Get token from memory, disk, or start new auth flow
    (unless smudge-api-oauth2-token
      (setq smudge-api-oauth2-token
            (or (smudge-api-oauth2-load-token)
                (smudge-api-oauth2-auth
                 smudge-api-oauth2-auth-url
                 smudge-api-oauth2-token-url
                 smudge-oauth2-client-id
                 smudge-oauth2-client-secret
                 smudge-api-oauth2-scopes
                 (format "%s://%s:%s/%s"
                         smudge-oauth2-callback-scheme
                         smudge-oauth2-callback-host
                         smudge-oauth2-callback-port
                         smudge-oauth2-callback-endpoint)))))

    ;; Refresh token if expired
    (when smudge-api-oauth2-token
      (setq smudge-api-oauth2-token
            (oauth2-refresh-access smudge-api-oauth2-token "smudge")))

    smudge-api-oauth2-token))

(defun smudge-api-call-async (method uri &optional data callback)
  "Make a request to the given Spotify service endpoint URI via METHOD.
Call CALLBACK with the parsed JSON response."
  (request (concat smudge-api-endpoint uri)
    :headers `(("Authorization" .
                ,(format "Bearer %s" (oauth2-token-access-token (smudge-api-oauth2-token))))
               ("Accept" . "application/json")
               ("Content-Type" . "application/json")
               ("Content-Length" . ,(number-to-string (length data))))
    :type method
    :parser (lambda ()
              (condition-case nil
                  (json-parse-buffer
                   :object-type 'hash-table
                   :array-type 'list)
                (json-parse-error nil)
                (json-end-of-file nil)))
    :encoding 'utf-8
    :data data
    :success (cl-function
              (lambda (&rest data &key response &allow-other-keys)
                (when callback (funcall callback (request-response-data response)))))

    :error (cl-function
            (lambda (&rest args &key error-thrown &allow-other-keys)
              (message "Got error: %S" error-thrown)))))

(defun smudge-api-current-user (callback)
  "Call CALLBACK with the currently logged in user."
  (if smudge-user
      (funcall callback smudge-user)
    (smudge-api-call-async
     "GET"
     "/me"
     nil
     (lambda (user)
       (setq smudge-user user)
       (funcall callback user)))))

(defun smudge-api-get-items (json)
  "Return the list of items from the given JSON object."
  (gethash "items" json))

(defun smudge-api-get-search-track-items (json)
  "Return track items from the given search results JSON object."
  (smudge-api-get-items (gethash "tracks" json)))

(defun smudge-api-get-search-playlist-items (json)
  "Return playlist items from the given search results JSON object."
  (smudge-api-get-items (gethash "playlists" json)))

(defun smudge-api-get-message (json)
  "Return the message from the featured playlists JSON object."
  (gethash "message" json))

(defun smudge-api-get-playlist-tracks (json)
  "Return the list of tracks from the given playlist JSON object."
  (mapcar (lambda (item)
            (gethash "track" item))
          (smudge-api-get-items json)))

(defun smudge-api-get-track-album (json)
  "Return the simplified album object from the given track JSON object."
  (gethash "album" json))

(defun smudge-api-get-track-number (json)
  "Return the track number from the given track JSON object."
  (gethash "track_number" json))

(defun smudge-api-get-disc-number (json)
  "Return the disc number from the given track JSON object."
  (gethash "disc_number" json))

(defun smudge-api-get-track-duration (json)
  "Return the track duration, in milliseconds, from the given track JSON object."
  (gethash "duration_ms" json))

(defun smudge-api-get-track-duration-formatted (json)
  "Return the formatted track duration from the given track JSON object."
  (format-seconds "%m:%02s" (/ (smudge-api-get-track-duration json) 1000)))

(defun smudge-api-get-track-album-name (json)
  "Return the album name from the given track JSON object."
  (smudge-api-get-item-name (smudge-api-get-track-album json)))

(defun smudge-api-get-track-artist (json)
  "Return the first simplified artist object from the given track JSON object."
  (car (gethash "artists" json)))

(defun smudge-api-get-track-artist-name (json)
  "Return the first artist name from the given track JSON object."
  (smudge-api-get-item-name (smudge-api-get-track-artist json)))

(defun smudge-api-get-track-popularity (json)
  "Return the popularity from the given track/album/artist JSON object."
  (gethash "popularity" json))

(defun smudge-api-is-track-playable (json)
  "Return whether the given track JSON object is playable by the current user."
  (not (eq :false (gethash "is_playable" json))))

(defun smudge-api-get-item-name (json)
  "Return the name from the given track/album/artist JSON object."
  (gethash "name" json))

(defun smudge-api-get-item-id (json)
  "Return the id from the given JSON object."
  (gethash "id" json))

(defun smudge-api-get-item-uri (json)
  "Return the uri from the given track/album/artist JSON object."
  (gethash "uri" json))

(defun smudge-api-get-playlist-track-count (json)
  "Return the number of tracks of the given playlist JSON object."
  (gethash "total" (gethash "tracks" json)))

(defun smudge-api-get-playlist-owner-id (json)
  "Return the owner id of the given playlist JSON object."
  (smudge-api-get-item-id (gethash "owner" json)))

(defun smudge-api-search (type query page callback)
  "Search artists, albums, tracks or playlists.
Call CALLBACK with PAGE of items that match QUERY, depending on TYPE."
  (let ((offset (* smudge-api-search-limit (1- page))))
    (smudge-api-call-async
     "GET"
     (concat "/search?"
             (url-build-query-string `((q      ,query)
                                       (type   ,type)
                                       (limit  ,smudge-api-search-limit)
                                       (offset ,offset)
                                       (market from_token))
                                     nil t))
     nil
     callback)))

(defun smudge-api-user-playlists (user-id page callback)
  "Call CALLBACK with the PAGE of playlists for the given USER-ID."
  (let ((offset (* smudge-api-search-limit (1- page))))
    (smudge-api-call-async
     "GET"
     (concat (format "/users/%s/playlists?" (url-hexify-string user-id))
             (url-build-query-string `((limit  ,smudge-api-search-limit)
                                       (offset ,offset))
                                     nil t))
     nil
     callback)))

(defun smudge-api-playlist-create (user-id name public callback)
  "Create a new playlist with NAME for the given USER-ID.
Make PUBLIC if true.  Call CALLBACK with results"
  (smudge-api-call-async
   "POST"
   (format "/users/%s/playlists" (url-hexify-string user-id))
   (format "{\"name\":\"%s\",\"public\":\"%s\"}" name (if public "true" "false"))
   callback))

(defun smudge-api-playlist-add-track (user-id playlist-id track-id callback)
  "Add TRACK-ID to PLAYLIST-ID.
Added by USER-ID.  Call CALLBACK with results."
  (smudge-api-playlist-add-tracks user-id playlist-id (list track-id) callback))

(defun smudge-api-format-id (type id)
  "Format ID.  Wrap with TYPE if necessary."
  (if (string-match-p "spotify" id)
      (format "\"%s\"" id)
    (format "\"spotify:%s:%s\"" type id)))

(defun smudge-api-playlist-add-tracks (user-id playlist-id track-ids callback)
  "Add TRACK-IDs to PLAYLIST-ID for USER-ID.
Call CALLBACK with results."
  (let ((tracks (format "%s" (mapconcat (lambda (x) (smudge-api-format-id "track" x)) track-ids ","))))
    (smudge-api-call-async
     "POST"
     (format "/users/%s/playlists/%s/tracks"
             (url-hexify-string user-id) (url-hexify-string playlist-id))
     (format "{\"uris\": [ %s ]}" tracks)
     callback)))

(defun smudge-api-playlist-remove-track (playlist-id track-id callback)
  "Remove TRACK-ID from PLAYLIST-ID.
Removed by USER-ID. Call CALLBACK with results."
  (smudge-api-playlist-remove-tracks playlist-id (list track-id) callback))

(defun smudge-api-playlist-remove-tracks (playlist-id track-ids callback)
  "Remove TRACK-IDS from PLAYLIST-ID for USER-ID.
Call CALLBACK with results."
  (let ((tracks (format "%s" (mapconcat
                              (lambda (x) (format "{\"uri\": %s}" (smudge-api-format-id "track" x)))
                              track-ids ","))))
    (smudge-api-call-async
     "DELETE"
     (format "/playlists/%s/tracks" (url-hexify-string playlist-id))
     (format "{\"tracks\": [ %s ]}" tracks)
     callback)))

(defun smudge-api-playlist-follow (playlist callback)
  "Add the current user as a follower of PLAYLIST.
Call CALLBACK with results."
  (let ((owner (smudge-api-get-playlist-owner-id playlist))
        (id (smudge-api-get-item-id playlist)))
    (smudge-api-call-async
     "PUT"
     (format "/users/%s/playlists/%s/followers"
             (url-hexify-string owner)
             (url-hexify-string id))
     nil
     callback)))

(defun smudge-api-playlist-unfollow (playlist callback)
  "Remove the current user as a follower of PLAYLIST.
Call CALLBACK with results."
  (let ((owner (smudge-api-get-playlist-owner-id playlist))
        (id (smudge-api-get-item-id playlist)))
    (smudge-api-call-async
     "DELETE"
     (format "/users/%s/playlists/%s/followers"
             (url-hexify-string owner)
             (url-hexify-string id))
     nil
     callback)))

(defun smudge-api-playlist-tracks (playlist page callback)
  "Call CALLBACK with PAGE of results of tracks from PLAYLIST."
  (let ((owner (smudge-api-get-playlist-owner-id playlist))
        (id (smudge-api-get-item-id playlist))
        (offset (* smudge-api-search-limit (1- page))))
    (smudge-api-call-async
     "GET"
     (concat (format "/users/%s/playlists/%s/tracks?"
                     (url-hexify-string owner)
                     (url-hexify-string id))
             (url-build-query-string `((limit  ,smudge-api-search-limit)
                                       (offset ,offset)
                                       (market from_token))
                                     nil t))
     nil
     callback)))

(defun smudge-api-album-tracks (album page callback)
  "Call CALLBACK with PAGE of tracks for ALBUM."
  (let ((album-id (smudge-api-get-item-id album))
        (offset (* smudge-api-search-limit (1- page))))
    (smudge-api-call-async
     "GET"
     (concat (format "/albums/%s/tracks?"
                     (url-hexify-string album-id))
             (url-build-query-string `((limit ,smudge-api-search-limit)
                                       (offset ,offset)
                                       (market from_token))
                                     nil t))
     nil
     callback)))

(defun smudge-api-popularity-bar (popularity)
  "Return the popularity indicator bar proportional to POPULARITY.
Parameter must be a number between 0 and 100."
  (let ((num-bars (truncate (/ popularity 10))))
    (concat (make-string num-bars ?X)
            (make-string (- 10 num-bars) ?-))))

(defun smudge-api-recently-played (page callback)
  "Call CALLBACK with PAGE of recently played tracks."
  (let ((offset (* smudge-api-search-limit (1- page))))
    (smudge-api-call-async
     "GET"
     (concat "/me/player/recently-played?"
             (url-build-query-string `((limit  ,smudge-api-search-limit)
                                       (offset ,offset))
                                     nil t))
     nil
     callback)))

(defun smudge-api-device-list (callback)
  "Call CALLBACK with the list of devices available for use with Spotify Connect."
  (smudge-api-call-async
   "GET"
   "/me/player/devices"
   nil
   callback))

(defun smudge-api-transfer-player (device-id &optional callback)
  "Transfer playback to DEVICE-ID and determine if it should start playing.
Call CALLBACK with result if provided."
  (smudge-api-call-async
   "PUT"
   "/me/player"
   (format "{\"device_ids\":[\"%s\"]}" device-id)
   callback))

(defun smudge-api-set-volume (device-id percentage &optional callback)
  "Set the volume level to PERCENTAGE of max for DEVICE-ID."
  (smudge-api-call-async
   "PUT"
   (concat "/me/player/volume?"
           (url-build-query-string `((volume_percent ,percentage)
                                     (device_id      ,device-id))
                                   nil t))
   nil
   callback))

(defun smudge-api-get-player-status (callback)
  "Call CALLBACK with the Spotify Connect status of the currently active player."
  (smudge-api-call-async
   "GET"
   "/me/player"
   nil
   callback))

(defun smudge-api-play (&optional callback uri context)
  "Play a track.  If no args, resume playing current track.
Otherwise, play URI in CONTEXT.  Call CALLBACK with results if provided."
  (smudge-api-call-async
   "PUT"
   "/me/player/play"
   (concat " { "
           (cond ((and uri context) (format "\"context_uri\": \"%s\", \"offset\": {\"uri\": \"%s\"}" context uri))
                 (context           (format "\"context_uri\": \"%s\"" context))
                 (uri               (format "\"uris\": [ \"%s\" ]" uri))
                 (t                  ""))
           " } ")
   callback))

(defun smudge-api-pause (&optional callback)
  "Pause the currently playing track.
Call CALLBACK if provided."
  (smudge-api-call-async
   "PUT"
   "/me/player/pause"
   nil
   callback))

(defun smudge-api-next (&optional callback)
  "Skip to the next track.
Call CALLBACK if provided."
  (smudge-api-call-async
   "POST"
   "/me/player/next"
   nil
   callback))

(defun smudge-api-previous (&optional callback)
  "Skip to the previous track.
Call CALLBACK if provided."
  (smudge-api-call-async
   "POST"
   "/me/player/previous"
   nil
   callback))

(defun smudge-api-repeat (state &optional callback)
  "Set repeat of current track to STATE.
Call CALLBACK if provided."
  (smudge-api-call-async
   "PUT"
   (concat "/me/player/repeat?"
           (url-build-query-string `((state ,state))
                                   nil t))
   nil
   callback))

(defun smudge-api-shuffle (state &optional callback)
  "Set repeat of current track to STATE.
Call CALLBACK if provided."
  (smudge-api-call-async
   "PUT"
   (concat "/me/player/shuffle?"
           (url-build-query-string `((state ,state))
                                   nil t))
   nil
   callback))


(defun smudge-api-queue-add-track (track-id &optional callback)
  "Add given TRACK-ID to the queue and call CALLBACK afterwards."
  (smudge-api-call-async
   "POST"
   (concat "/me/player/queue?"
           (url-build-query-string `((uri ,track-id))
                                   nil t))
   nil
   callback))

(defun smudge-api-queue-add-tracks (track-ids &optional callback)
  "Add given TRACK-IDS to the queue and call CALLBACK afterwards."
  ;; Spotify's API doesn't provide a endpoint that would enable us to
  ;; add multiple tracks to the queue at the same time.
  ;; Thus we have to synchronously add the tracks
  ;; one by one to the queue.
  (if (car track-ids)
      (smudge-api-queue-add-track
       (car track-ids)
       (lambda (_)
         (smudge-api-queue-add-tracks (cdr track-ids) callback)))
    (when callback (funcall callback))))

(defun smudge-api-save-tracks-to-my-library (track-ids &optional callback)
  "Save one or more TRACK-IDS to the user's \"Liked Songs\" library.

Up to 50 tracks can be specified per API call.

Calls CALLBACK function with the API response."
  (smudge-api-call-async
   "PUT"
   (concat "/me/tracks?ids=" (string-join track-ids ","))
   nil
   callback))

(defun smudge-api-remove-tracks-from-my-library (track-ids &optional callback)
  "Save one or more TRACK-IDS to the user's \"Liked Songs\" library.

Up to 50 tracks can be specified per API call.

Calls CALLBACK function with the API response."
  (smudge-api-call-async
   "DELETE"
   (concat "/me/tracks?ids=" (string-join track-ids ","))
   nil
   callback))

(defun smudge-api-get-my-library-tracks (page callback)
  "Get PAGE of songs saved in the user's \"Liked Songs\"library.

Calls CALLBACK function with the API response."
  (let ((offset (* smudge-api-search-limit (1- page))))
    (smudge-api-call-async
     "GET"
     (concat "/me/tracks?"
             (url-build-query-string `((limit ,smudge-api-search-limit)
                                       (offset ,offset)
                                       (market from_token))))
     nil
     callback)))

(provide 'smudge-api)
;;; smudge-api.el ends here
