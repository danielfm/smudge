;; spotify-api.el --- Spotify.el API integration layer

;; Copyright (C) 2014-2019 Daniel Fernandes Martins

;; Code:

(defvar *spotify-user*         nil "Cached user object")
(defvar *spotify-oauth2-token* nil "Cached OAuth2 token")
(defvar *spotify-oauth2-ts*    nil "Unix timestamp in which the OAuth2 token was retrieved. This is used to manually refresh the token when it's about to expire.")

(defconst spotify-api-endpoint     "https://api.spotify.com/v1")
(defconst spotify-oauth2-auth-url  "https://accounts.spotify.com/authorize")
(defconst spotify-oauth2-token-url "https://accounts.spotify.com/api/token")
(defconst spotify-oauth2-scopes    "playlist-read-private playlist-modify-public playlist-modify-private user-read-private user-read-playback-state user-modify-playback-state user-read-playback-state user-read-recently-played")
(defconst spotify-oauth2-callback  "http://localhost:8591/")

(defcustom spotify-oauth2-client-id ""
  "The unique identifier for your application. More info at
https://developer.spotify.com/web-api/tutorial/."
  :type 'string)

(defcustom spotify-oauth2-client-secret ""
  "The key that you will need to pass in secure calls to the Spotify Accounts and
Web API services. More info at
https://developer.spotify.com/web-api/tutorial/."
  :type 'string)

(defcustom spotify-api-search-limit 50
  "Number of items returned when searching for something using the Spotify API."
  :type 'integer)

(defcustom spotify-api-locale "en_US"
  "Optional. The desired language, consisting of an ISO 639 language code and
an ISO 3166-1 alpha-2 country code, joined by an underscore.
For example: es_MX, meaning Spanish (Mexico). Provide this parameter if you
want the category metadata returned in a particular language."
  :type 'string)

(defcustom spotify-api-country "US"
  "Optional. A country: an ISO 3166-1 alpha-2 country code. Provide this
parameter if you want to narrow the list of returned categories to those
relevant to a particular country. If omitted, the returned items will be
globally relevant."
  :type 'string)

;; Do not rely on the auto-refresh logic from oauth2.el, which seems broken for async requests
(defun spotify-oauth2-token ()
  "Retrieve the Oauth2 access token that must be used to interact with the
Spotify API."
  (let ((now (string-to-number (format-time-string "%s"))))
    (if (null *spotify-oauth2-token*)
        (let ((token (oauth2-auth spotify-oauth2-auth-url
                                  spotify-oauth2-token-url
                                  spotify-oauth2-client-id
                                  spotify-oauth2-client-secret
                                  spotify-oauth2-scopes
                                  nil
                                  spotify-oauth2-callback)))
          (setq *spotify-oauth2-token* token)
          (setq *spotify-oauth2-ts* now)
          (if (null token)
              (user-error "OAuth2 authentication failed")
            token))
      (if (> now (+ *spotify-oauth2-ts* 3000))
          (let ((token (oauth2-refresh-access *spotify-oauth2-token*)))
            (setq *spotify-oauth2-token* token)
            (setq *spotify-oauth2-ts* now)
            (if (null token)
                (user-error "Could not refresh OAuth2 token")
              token))
        *spotify-oauth2-token*))))

(defun spotify-api-call-async (method uri &optional data callback is-retry)
  "Make a request to the given Spotify service endpoint and calls CALLBACK with
the parsed JSON response."
  (lexical-let ((method method)
                (uri uri)
                (data data)
                (callback callback)
                (is-retry is-retry))
    (oauth2-url-retrieve
     (spotify-oauth2-token)
     (concat spotify-api-endpoint uri)
     (lambda (_)
       (toggle-enable-multibyte-characters t)
       (goto-char (point-min))
       (condition-case err
           (when (search-forward-regexp "^$" nil t)
             (let* ((json-object-type 'hash-table)
                    (json-array-type 'list)
                    (json-key-type 'symbol)
                    (json (json-read))
                    (error-json (gethash 'error json)))
               (kill-buffer)

               ;; Retries the request when the token expires and gets refreshed
               (if (and (hash-table-p error-json)
                        (eq 401 (gethash 'status error-json))
                        (not is-retry))
                   (spotify-api-call-async method uri data callback t)
                 (when callback (funcall callback json)))))

         ;; Handle empty responses
         (end-of-file
          (kill-buffer)
          (when callback (funcall callback nil)))))
     nil
     method
     (or data "")
     '(("Content-Type" . "application/json")))))

(defun spotify-current-user (callback)
  ""
  (if *spotify-user*
      (funcall callback *spotify-user*)
    (lexical-let ((callback callback))
      (spotify-api-call-async
       "GET"
       "/me"
       nil
       (lambda (user)
         (setq *spotify-user* user)
         (funcall callback user))))))

(defun spotify-get-items (json)
  "Return the list of items from the given JSON object."
  (gethash 'items json))

(defun spotify-get-search-track-items (json)
  "Return track items from the given search results JSON object."
  (spotify-get-items (gethash 'tracks json)))

(defun spotify-get-search-playlist-items (json)
  "Return playlist items from the given search results JSON object."
  (spotify-get-items (gethash 'playlists json)))

(defun spotify-get-message (json)
  "Return the message from the featured playlists JSON object."
  (gethash 'message json))

(defun spotify-get-playlist-tracks (json)
  "Return the list of tracks from the given playlist JSON object."
  (mapcar #'(lambda (item)
              (gethash 'track item))
          (spotify-get-items json)))

(defun spotify-get-search-playlist-items (json)
  "Return the playlist items from the given search results JSON object."
  (spotify-get-items (gethash 'playlists json)))

(defun spotify-get-track-album (json)
  "Return the simplified album object from the given track JSON object."
  (gethash 'album json))

(defun spotify-get-track-number (json)
  "Return the track number from the given track JSON object."
  (gethash 'track_number json))

(defun spotify-get-disc-number (json)
  "Return the disc number from the given track JSON object."
  (gethash 'disc_number json))

(defun spotify-get-track-duration (json)
  "Return the track duration, in milliseconds, from the given track JSON object."
  (gethash 'duration_ms json))

(defun spotify-get-track-duration-formatted (json)
  "Return the formatted track duration from the given track JSON object."
  (format-seconds "%m:%02s" (/ (spotify-get-track-duration json) 1000)))

(defun spotify-get-track-album-name (json)
  "Return the album name from the given track JSON object."
  (spotify-get-item-name (spotify-get-track-album json)))

(defun spotify-get-track-artist (json)
  "Return the first simplified artist object from the given track JSON object."
  (first (gethash 'artists json)))

(defun spotify-get-track-artist-name (json)
  "Return the first artist name from the given track JSON object."
  (spotify-get-item-name (spotify-get-track-artist json)))

(defun spotify-get-track-popularity (json)
  "Return the popularity from the given track/album/artist JSON object."
  (gethash 'popularity json))

(defun spotify-is-track-playable (json)
  "Return whether the given track JSON object represents a playable track by
the current user."
  (not (eq :json-false (gethash 'is_playable json))))

(defun spotify-get-item-name (json)
  "Return the name from the given track/album/artist JSON object."
  (gethash 'name json))

(defun spotify-get-item-id (json)
  "Return the id from the given JSON object."
  (gethash 'id json))

(defun spotify-get-item-uri (json)
  "Return the uri from the given track/album/artist JSON object."
  (gethash 'uri json))

(defun spotify-get-playlist-track-count (json)
  "Return the number of tracks of the given playlist JSON object."
  (gethash 'total (gethash 'tracks json)))

(defun spotify-get-playlist-owner-id (json)
  "Return the owner id of the given playlist JSON object."
  (spotify-get-item-id (gethash 'owner json)))

(defun spotify-api-search (type query page callback)
  "Search artists, albums, tracks or playlists that match a keyword string,
depending on the `type' argument."
  (let ((offset (* spotify-api-search-limit (1- page))))
    (spotify-api-call-async
     "GET"
     (concat "/search?"
             (url-build-query-string `((q      ,query)
                                       (type   ,type)
                                       (limit  ,spotify-api-search-limit)
                                       (offset ,offset)
                                       (market from_token))
                                     nil t))
     nil
     callback)))

(defun spotify-api-featured-playlists (page callback)
  "Return the given page of Spotify's featured playlists."
  (let ((offset (* spotify-api-search-limit (1- page))))
    (spotify-api-call-async
     "GET"
     (concat "/browse/featured-playlists?"
             (url-build-query-string `((locale  ,spotify-api-locale)
                                       (country ,spotify-api-country)
                                       (limit   ,spotify-api-search-limit)
                                       (offset  ,offset))
                                     nil t))
     nil
     callback)))

(defun spotify-api-user-playlists (user-id page callback)
  "Return the playlists for the given user."
  (let ((offset (* spotify-api-search-limit (1- page))))
    (spotify-api-call-async
     "GET"
     (concat (format "/users/%s/playlists?" (url-hexify-string user-id))
             (url-build-query-string `((limit  ,spotify-api-search-limit)
                                       (offset ,offset))
                                     nil t))
     nil
     callback)))

(defun spotify-api-playlist-create (user-id name is-public callback)
  "Create a new playlist with the given name for the given user."
  (spotify-api-call-async
   "POST"
   (format "/users/%s/playlists" (url-hexify-string user-id))
   (format "{\"name\":\"%s\",\"public\":\"%s\"}" name (if is-public "true" "false"))
   callback))

(defun spotify-api-playlist-add-track (user-id playlist-id track-id callback)
  "Add single track to playlist."
  (spotify-api-playlist-add-tracks user-id playlist-id (list track-id) callback))

(defun spotify-format-id (type id)
  "Wrap raw id to type if necessary."
   (if (string-match-p "spotify" id) (format "\"%s\"" id) (format "\"spotify:%s:%s\"" type id)))

(defun spotify-api-playlist-add-tracks (user-id playlist-id track-ids callback)
  "Add tracks in list track-ids in playlist."
  (let ((tracks (format "%s" (mapconcat (lambda (x) (spotify-format-id "track" x)) track-ids ","))))
    (spotify-api-call-async
     "POST"
     (format "/users/%s/playlists/%s/tracks"
             (url-hexify-string user-id) (url-hexify-string playlist-id))
     (format "{\"uris\": [ %s ]}" tracks)
     callback)))

(defun spotify-api-playlist-follow (playlist callback)
  "Add the current user as a follower of a playlist."
  (let ((owner (spotify-get-playlist-owner-id playlist))
        (id (spotify-get-item-id playlist)))
    (spotify-api-call-async
     "PUT"
     (format "/users/%s/playlists/%s/followers"
             (url-hexify-string owner)
             (url-hexify-string id))
     nil
     callback)))

(defun spotify-api-playlist-unfollow (playlist callback)
  "Remove the current user as a follower of a playlist."
  (let ((owner (spotify-get-playlist-owner-id playlist))
        (id (spotify-get-item-id playlist)))
    (spotify-api-call-async
     "DELETE"
     (format "/users/%s/playlists/%s/followers"
             (url-hexify-string owner)
             (url-hexify-string id))
     nil
     callback)))

(defun spotify-api-playlist-tracks (playlist page callback)
  "Return the tracks of the given user's playlist."
  (let ((owner (spotify-get-playlist-owner-id playlist))
        (id (spotify-get-item-id playlist))
        (offset (* spotify-api-search-limit (1- page))))
    (spotify-api-call-async
     "GET"
     (concat (format "/users/%s/playlists/%s/tracks?"
                     (url-hexify-string owner)
                     (url-hexify-string id))
             (url-build-query-string `((limit  ,spotify-api-search-limit)
                                       (offset ,offset)
                                       (market from_token))
                                     nil t))
     nil
     callback)))

(defun spotify-api-album-tracks (album page callback)
  "Return the tracks for the given album."
  (let ((album-id (spotify-get-item-id album))
        (offset (* spotify-api-search-limit (1- page))))
    (spotify-api-call-async
     "GET"
     (concat (format "/albums/%s/tracks?"
                     (url-hexify-string album-id))
             (url-build-query-string `((limit ,spotify-api-search-limit)
                                       (offset ,offset)
                                       (market from_token))
                                     nil t))
     nil
     callback)))

(defun spotify-popularity-bar (popularity)
  "Return the popularity indicator bar proportional to the given parameter,
which must be a number between 0 and 100."
  (let ((num-bars (truncate (/ popularity 10))))
    (concat (make-string num-bars ?X)
            (make-string (- 10 num-bars) ?-))))

(defun spotify-api-recently-played (page callback)
  "Retrieve the list of recently played tracks."
  (let ((offset (* spotify-api-search-limit (1- page))))
    (spotify-api-call-async
     "GET"
     (concat "/me/player/recently-played?"
             (url-build-query-string `((limit  ,spotify-api-search-limit)
                                       (offset ,offset))
                                     nil t))
     nil
     callback)))

(defun spotify-api-device-list (callback)
  "Call CALLBACK with the list of devices available for use with Spotify Connect."
  (lexical-let ((callback callback))
    (spotify-api-call-async
     "GET"
     "/me/player/devices"
     nil
     callback)))

(defun spotify-api-transfer-player (device-id &optional callback)
  "Transfer playback to DEVICE-ID and determine if it should start playing."
  (spotify-api-call-async
   "PUT"
   "/me/player"
   (format "{\"device_ids\":[\"%s\"]}" device-id)
   callback))

(defun spotify-api-set-volume (device-id percentage &optional callback)
  "Set the volume level to PERCENTAGE of max for DEVICE-ID."
  (spotify-api-call-async
   "PUT"
   (concat "/me/player/volume?"
           (url-build-query-string `((volume_percent ,percentage)
                                     (device_id      ,device-id))
                                   nil t))
   nil
   callback))

(defun spotify-api-get-player-status (callback)
  "Get the Spotify Connect status of the currently active player."
  (spotify-api-call-async
   "GET"
   "/me/player"
   nil
   callback))

(defun spotify-api-play (&optional callback uri context)
  "Play a track. If no args, resume playing current track. Otherwise, play URI in CONTEXT."
  (spotify-api-call-async
   "PUT"
   "/me/player/play"
   (concat " { "
           (cond ((and uri context) (format "\"context_uri\": \"%s\", \"offset\": {\"uri\": \"%s\"}" context uri))
                 (context           (format "\"context_uri\": \"%s\"" context))
                 (uri               (format "\"uris\": [ \"%s\" ]" uri))
                 (t                  ""))
           " } ")
   callback))

(defun spotify-api-pause (&optional callback)
  "Pause the currently playing track."
  (spotify-api-call-async
   "PUT"
   "/me/player/pause"
   nil
   callback))

(defun spotify-api-next (&optional callback)
  "Skip to the next track."
  (spotify-api-call-async
   "POST"
   "/me/player/next"
   nil
   callback))

(defun spotify-api-previous (&optional callback)
  "Skip to the previous track."
  (spotify-api-call-async
   "POST"
   "/me/player/previous"
   nil
   callback))

(defun spotify-api-repeat (state &optional callback)
  "Set repeat of current track to STATE."
  (spotify-api-call-async
   "PUT"
   (concat "/me/player/repeat?"
           (url-build-query-string `((state ,state))
                                   nil t))
   nil
   callback))

(defun spotify-api-shuffle (state &optional callback)
  "Set repeat of current track to STATE."
  (spotify-api-call-async
   "PUT"
   (concat "/me/player/shuffle?"
           (url-build-query-string `((state ,state))
                                   nil t))
   nil
   callback))


(provide 'spotify-api)
