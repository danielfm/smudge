;; spotify-api.el --- Spotify.el API integration layer

;; Copyright (C) 2014-2018 Daniel Fernandes Martins

;; Code:

(defvar *spotify-oauth2-token* nil "Cached OAuth2 token")
(defvar *spotify-user* nil "Cached user object")

(defconst spotify-api-endpoint     "https://api.spotify.com/v1")
(defconst spotify-oauth2-auth-url  "https://accounts.spotify.com/authorize")
(defconst spotify-oauth2-token-url "https://accounts.spotify.com/api/token")
(defconst spotify-oauth2-scopes    "playlist-read-private playlist-modify-public playlist-modify-private user-read-private")
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

(defun spotify-retrieve-oauth2-token ()
  "Retrieves the Oauth2 access token that must be used to interact with the
Spotify API."
  (if *spotify-oauth2-token*
      *spotify-oauth2-token*
    (let ((token (oauth2-auth spotify-oauth2-auth-url
                              spotify-oauth2-token-url
                              spotify-oauth2-client-id
                              spotify-oauth2-client-secret
                              spotify-oauth2-scopes
                              nil
                              spotify-oauth2-callback)))
      (setq *spotify-oauth2-token* token)
      (when (null token)
        (user-error "OAuth2 authentication failed"))
      token)))

(defun spotify-current-user ()
  "Retrieves the object that represents the authenticated user."
  (if *spotify-user*
      *spotify-user*
    (let ((user (spotify-api-call "GET" "/me")))
      (setq *spotify-user* user)
      user)))

(defun spotify-api-call (method uri &optional data is-retry)
  "Makes a request to the given Spotify service endpoint and returns the parsed
JSON response."
  (let ((url (concat spotify-api-endpoint uri))
        (headers '(("Content-Type" . "application/json"))))
    (with-current-buffer (oauth2-url-retrieve-synchronously (spotify-retrieve-oauth2-token)
                                                            url method data headers)
      (toggle-enable-multibyte-characters t)
      (goto-char (point-min))

      ;; If (json-read) signals 'end-of-file, we still kill the temp buffer
      ;; and re-signal the error
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
                  (spotify-api-call method uri data t)
                json)))
        (end-of-file
         (kill-buffer)
         (signal (car err) (cdr err)))))))

(defun spotify-current-user-name ()
  "Returns the user's display name of the current Spotify session."
  (gethash 'display_name (spotify-current-user)))

(defun spotify-current-user-id ()
  "Returns the user's id of the current Spotify session."
  (spotify-get-item-id (spotify-current-user)))

(defun spotify-get-items (json)
  "Returns the list of items from the given json object."
  (gethash 'items json))

(defun spotify-get-search-track-items (json)
  "Returns track items from the given search results json."
  (spotify-get-items (gethash 'tracks json)))

(defun spotify-get-search-playlist-items (json)
  "Returns playlist items from the given search results json."
  (spotify-get-items (gethash 'playlists json)))

(defun spotify-get-message (json)
  "Returns the message from the featured playlists response."
  (gethash 'message json))

(defun spotify-get-playlist-tracks (json)
  (mapcar #'(lambda (item)
              (gethash 'track item))
          (spotify-get-items json)))

(defun spotify-get-search-playlist-items (json)
  "Returns the playlist items from the given search results json."
  (spotify-get-items (gethash 'playlists json)))

(defun spotify-get-track-album (json)
  "Returns the simplified album object from the given track object."
  (gethash 'album json))

(defun spotify-get-track-number (json)
  "Returns the track number from the given track object."
  (gethash 'track_number json))

(defun spotify-get-track-duration (json)
  "Returns the track duration, in milliseconds, from the given track object."
  (gethash 'duration_ms json))

(defun spotify-get-track-duration-formatted (json)
  "Returns the formatted track duration from the given track object."
  (format-seconds "%m:%02s" (/ (spotify-get-track-duration json) 1000)))

(defun spotify-get-track-album-name (json)
  "Returns the album name from the given track object."
  (spotify-get-item-name (spotify-get-track-album json)))

(defun spotify-get-track-artist (json)
  "Returns the first artist from the given track object."
  (spotify-get-item-name (first (gethash 'artists json))))

(defun spotify-get-track-popularity (json)
  "Returns the popularity from the given track/album/artist object."
  (gethash 'popularity json))

(defun spotify-is-track-playable (json)
  "Returns whether the given track is playable by the current user."
  (not (eq :json-false (gethash 'is_playable json))))

(defun spotify-get-item-name (json)
  "Returns the name from the given track/album/artist object."
  (gethash 'name json))

(defun spotify-get-item-id (json)
  "Returns the id from the givem object."
  (gethash 'id json))

(defun spotify-get-item-uri (json)
  "Returns the uri from the given track/album/artist object."
  (gethash 'uri json))

(defun spotify-get-playlist-track-count (json)
  "Returns the number of tracks of the given playlist object."
  (gethash 'total (gethash 'tracks json)))

(defun spotify-get-playlist-owner-id (json)
  "Returns the owner id of the given playlist object."
  (spotify-get-item-id (gethash 'owner json)))

(defun spotify-api-search (type query page)
  "Searches artists, albums, tracks or playlists that match a keyword string,
depending on the `type' argument."
  (let ((offset (* spotify-api-search-limit (1- page))))
    (spotify-api-call "GET"
                      (concat "/search?"
                              (url-build-query-string `((q      ,query)
                                                        (type   ,type)
                                                        (limit  ,spotify-api-search-limit)
                                                        (offset ,offset)
                                                        (market from_token))
                                                      nil t)))))

(defun spotify-api-featured-playlists (page)
  "Returns the given page of Spotify's featured playlists."
  (let ((offset (* spotify-api-search-limit (1- page))))
    (spotify-api-call
     "GET"
     (concat "/browse/featured-playlists?"
             (url-build-query-string `((locale  ,spotify-api-locale)
                                       (country ,spotify-api-country)
                                       (limit   ,spotify-api-search-limit)
                                       (offset  ,offset))
                                     nil t)))))

(defun spotify-api-user-playlists (user-id page)
  "Returns the playlists for the given user."
  (let ((offset (* spotify-api-search-limit (1- page))))
    (spotify-api-call
     "GET"
     (concat (format "/users/%s/playlists?" (url-hexify-string user-id))
             (url-build-query-string `((limit  ,spotify-api-search-limit)
                                       (offset ,offset))
                                     nil t)))))

(defun spotify-api-playlist-create (user-id name is-public)
  "Creates a new playlist with the given name for the given user."
  (spotify-api-call
   "POST"
   (format "/users/%s/playlists"
           (url-hexify-string user-id))
   (format "{\"name\":\"%s\",\"public\":\"%s\"}"
           name
           (if is-public "true" "false"))))

(defun spotify-api-playlist-follow (playlist)
  "Adds the current user as a follower of a playlist."
  (condition-case err
      (let ((owner (spotify-get-playlist-owner-id playlist))
            (id (spotify-get-item-id playlist)))
        (spotify-api-call
         "PUT"
         (format "/users/%s/playlists/%s/followers"
                 (url-hexify-string owner)
                 (url-hexify-string id))
         ""))
    (end-of-file t)))

(defun spotify-api-playlist-unfollow (playlist)
  "Removes the current user as a follower of a playlist."
  (condition-case err
      (let ((owner (spotify-get-playlist-owner-id playlist))
            (id (spotify-get-item-id playlist)))
        (spotify-api-call
         "DELETE"
         (format "/users/%s/playlists/%s/followers"
                 (url-hexify-string owner)
                 (url-hexify-string id))
         ""))
    (end-of-file t)))

(defun spotify-api-playlist-tracks (playlist page)
  "Returns the tracks of the given user's playlist."
  (let ((owner (spotify-get-playlist-owner-id playlist))
        (id (spotify-get-item-id playlist))
        (offset (* spotify-api-search-limit (1- page))))
    (spotify-api-call
     "GET"
     (concat (format "/users/%s/playlists/%s/tracks?"
                     (url-hexify-string owner)
                     (url-hexify-string id)  offset)
             (url-build-query-string `((limit  ,spotify-api-search-limit)
                                       (offset ,offset)
                                       (market from_token))
                                     nil t)))))

(defun spotify-popularity-bar (popularity)
  "Returns the popularity indicator bar proportional to the given parameter,
which must be a number between 0 and 100."
  (let ((num-bars (truncate (/ popularity 10))))
    (concat (make-string num-bars ?X)
            (make-string (- 10 num-bars) ?-))))

(provide 'spotify-api)
