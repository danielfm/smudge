;; spotify-api.el --- Spotify.el API integration layer

;; Copyright (C) 2014 Daniel Fernandes Martins

;; Code:

(defvar *spotify-oauth2-token*)
(defvar *spotify-user*)

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

(defun spotify-api-auth ()
  "Starts the Spotify Oauth2 authentication and authorization workflow."
  (oauth2-auth-and-store spotify-oauth2-auth-url
                         spotify-oauth2-token-url
                         spotify-oauth2-scopes
                         spotify-oauth2-client-id
                         spotify-oauth2-client-secret
                         spotify-oauth2-callback))

  "Makes a request to the given Spotify service endpoint and returns the parsed
JSON response."
  (let ((url (concat spotify-api-endpoint uri))
        (headers '(("Content-Type" . "application/json"))))
    (with-current-buffer (oauth2-url-retrieve-synchronously *spotify-oauth2-token*
                                                            url method data headers)
      (toggle-enable-multibyte-characters t)
      (goto-char (point-min))
      (when (search-forward-regexp "^$" nil t)
        (let* ((json-object-type 'hash-table)
               (json-array-type 'list)
               (json-key-type 'symbol))
          (json-read))))))

(defun spotify-disconnect ()
  "Clears the Spotify session currently in use."
  (interactive)
  (makunbound '*spotify-oauth2-token*)
  (makunbound '*spotify-user*)
  (message "Spotify session closed"))

;;;###autoload
(defun spotify-connect ()
  "Starts a new Spotify session."
  (interactive)
  (spotify-disconnect)
  (defvar *spotify-oauth2-token* (spotify-api-auth))
  (defvar *spotify-user* (spotify-api-call "GET" "/me"))
  (when *spotify-user*
    (message "Welcome, %s!" (spotify-current-user-name))))

(defun spotify-current-user-name ()
  "Returns the user's display name of the current Spotify session."
  (gethash 'display_name *spotify-user*))

(defun spotify-current-user-id ()
  "Returns the user's id of the current Spotify session."
  (spotify-get-item-id *spotify-user*))

(defun spotify-get-items (json)
  "Returns the list of items from the given json object."
  (gethash 'items json))

(defun spotify-get-search-track-items (json)
  "Returns track items from the given search results json."
  (spotify-get-items (gethash 'tracks json)))

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

(defun spotify-get-track-album-name (json)
  "Returns the album name from the given track object."
  (spotify-get-item-name (spotify-get-track-album json)))

(defun spotify-get-track-artist (json)
  "Returns the first artist from the given track object."
  (spotify-get-item-name (first (gethash 'artists json))))

(defun spotify-get-track-popularity (json)
  "Returns the popularity from the given track/album/artist object."
  (gethash 'popularity json))

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
  (let ((escaped-query (url-hexify-string query))
        (offset (* spotify-api-search-limit (1- page))))
    (spotify-api-call "GET"
     (format "/search?q=%s&type=%s&limit=%d&offset=%d&market=from_token"
             escaped-query type spotify-api-search-limit offset))))

(defun spotify-api-user-playlists (user-id page)
  "Returns the playlists for the given user."
  (let ((offset (* spotify-api-search-limit (1- page))))
    (spotify-api-call
     "GET"
     (format "/users/%s/playlists?limit=%d&offset=%d"
             user-id spotify-api-search-limit offset))))

(defun spotify-api-playlist-create (user-id name is-public)
  "Creates a new playlist with the given name for the given user."
  (spotify-api-call
   "POST"
   (format "/users/%s/playlists"
           user-id)
   (format "{\"name\":\"%s\",\"public\":\"%s\"}"
           name
           (if is-public "true" "false"))))

(defun spotify-api-playlist-tracks (user-id playlist-id page)
  "Returns the tracks of the given user's playlist"
  (let ((offset (* spotify-api-search-limit (1- page))))
    (spotify-api-call
     "GET"
     (format "/users/%s/playlists/%s/tracks?limit=%d&offset=%d"
             user-id playlist-id spotify-api-search-limit offset))))

(defun spotify-popularity-bar (popularity)
  "Returns the popularity indicator bar proportional to the given parameter,
which must be a number between 0 and 100."
  (let ((num-bars (truncate (/ popularity 10))))
    (concat (make-string num-bars ?\u25cf)
            (make-string (- 10 num-bars) ?\u25cb))))

(provide 'spotify-api)
