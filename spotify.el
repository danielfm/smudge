;;; spotify.el --- control the Spotify app from Emacs

;; Copyright (C) 2014 Daniel Fernandes Martins

;; Keywords: multimedia, music, spotify
;; Package: spotify

;; Commentary:

;; This mode requires at least GNU Emacs 24.4 and Python 2.7

;; Before using this mode, first go the Spotify Web API console
;; <https://developer.spotify.com/my-applications> and create a new
;; application, adding <http://localhost:8591/> as the redirect URI.
;;
;; After requiring `spotify', make sure to define the client id and client
;; secrets, along with some other important settings:
;;
;; (custom-set-variables
;;  '(spotify-oauth2-client-id "client-id")
;;  '(spotify-oauth2-client-secret "client-secret")
;;
;;  ; Only for Mac OS X, for now
;;  '(spotify-transport 'apple) 
;;  '(spotify-osascript-bin-path "/usr/bin/osascript"))
;;
;; To authenticate, invoke the `spotify-connect' function. This will start the
;; Oauth2 authentication and authorization workflow. You may be asked to type
;; a password since the tokens are stored as an encrypted file in the local
;; filesystem. After you enter your credentials and authorizes the app, you
;; should see a greeting message in the echo area.
;;
;; To search for tracks, invoke the `spotify-track-search' function and
;; type your query. The results will be shown up in a new buffer. To play the
;; track under the cursor, just type RET, or type M-RET to play the
;; track's album from the start.

;; Code:

(require 'json)
(require 'oauth2)
(require 'tabulated-list)

(when (version< emacs-version "24.4")
  (error "Spotify requires at least GNU Emacs 24.4"))

(defgroup spotify nil
  "Spotify ckient."
  :version "0.0.1"
  :group 'multimedia)

;;; TODO: D-Bus support not implemented
(defcustom spotify-transport 'apple
  "How the commands should be sent to Spotify process."
  :type '(choice (symbol :tag "AppleScript" apple)
                 (symbol :tag "D-Bus" dbus)))

(defcustom spotify-osascript-bin-path "/usr/bin/osascript"
  "Path to `osascript' binary."
  :type 'string)

;; simple facility to emulate multimethods
(defun spotify-apply (suffix &rest args)
  (let ((func-name (format "spotify-%s-%s" spotify-transport suffix)))
    (apply (intern func-name) args)))

(defun spotify-playing-status-msg ()
  "Returns a string that describes whether Spotify is paused or playing."
  (message "Spotify is now %s"
           (if (spotify-playing-p) "playing" "paused")))

(defun spotify-play-track (context-id)
  "Sends a `play' command to Spotify process passing a context id."
  (interactive)
  (spotify-apply "player-play-track" context-id))

(defun spotify-play ()
  "Sends a `play' command to Spotify process."
  (interactive)
  (spotify-apply "player-play")
  (spotify-playing-status-msg))

(defun spotify-pause ()
  "Sends a `pause' command to Spotify process."
  (interactive)
  (spotify-apply "player-pause")
  (spotify-playing-status-msg))

(defun spotify-playing-p ()
  "Returns whether Spotify is playing."
  (interactive)
  (spotify-apply "player-playing-p"))

(defun spotify-repeating-p ()
  "Returns whether Spotify have repeating turned on."
  (interactive)
  (spotify-apply "repeating-p"))

(defun spotify-toggle-repeating ()
  "Sends a command to Spotify process to toggle the repeating flag."
  (interactive)
  (spotify-apply "toggle-repeating")
  (message "Spotify repeating is %s"
           (if (spotify-repeating-p) "on" "off")))

(defun spotify-shuffling-p ()
  "Returns whether Spotify have shuffling turned on."
  (interactive)
  (spotify-apply "shuffling-p"))

(defun spotify-toggle-shuffling ()
  "Sends a command to Spotify process to toggle the shuffling flag."
  (interactive)
  (spotify-apply "toggle-shuffling")
  (message "Spotify shuffling is %s"
           (if (spotify-shuffling-p) "on" "off")))

;;;
;;; D-Bus-specific code
;;;

;;; TODO: Not implemented yet (ever?)

;;;
;;; AppleScript-specific code
;;;

(defun spotify-apple-command-true-output-p (out-str)
  (string= "true" out-str))

(defun spotify-apple-command (cmd)
  (replace-regexp-in-string
   "\n$" ""
   (shell-command-to-string  
    (format "%s -e 'tell application \"Spotify\" to %s'" spotify-osascript-bin-path cmd))))

(defun spotify-apple-player-state ()
  (spotify-apple-command "get player state"))

(defun spotify-apple-player-play ()
  (spotify-apple-command "play"))

(defun spotify-apple-toggle-repeating ()
  (spotify-apple-command "set repeating to not repeating"))

(defun spotify-apple-toggle-shuffling ()
  (spotify-apple-command "set shuffling to not shuffling"))

(defun spotify-apple-player-play-track (context-id)
  (spotify-apple-command (format "play track \"%s\"" context-id)))

(defun spotify-apple-repeating-p ()
  (spotify-apple-command-true-output-p
   (spotify-apple-command "get repeating")))

(defun spotify-apple-shuffling-p ()
  (spotify-apple-command-true-output-p
   (spotify-apple-command "get shuffling")))

(defun spotify-apple-player-pause ()
  (spotify-apple-command "pause"))

(defun spotify-apple-player-playing-p ()
  (string= "playing" (spotify-apple-player-state)))

;;;
;;; OAuth2 API interface
;;;

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

(defun spotify-api-call (uri &optional method data)
  "Makes a request to the given Spotify service endpoint and returns the parsed
JSON response."
  (let ((url (concat spotify-api-endpoint uri)))
    (with-current-buffer (oauth2-url-retrieve-synchronously *spotify-oauth2-token*
                                                            url method data)
      (toggle-enable-multibyte-characters t)
      (goto-char (point-min))
      (when (search-forward-regexp "^$" nil t)
        (let* ((json-object-type 'hash-table)
               (json-array-type 'list)
               (json-key-type 'symbol))
          (prog1 (json-read)
            (kill-buffer)))))))

(defun spotify-disconnect ()
  "Clears the Spotify session currently in use."
  (interactive)
  (makunbound '*spotify-oauth2-token*)
  (makunbound '*spotify-user*))

;;;###autoload
(defun spotify-connect ()
  "Starts a new Spotify session."
  (interactive)
  (defvar *spotify-oauth2-token* (spotify-api-auth))
  (defvar *spotify-user* (spotify-api-call "/me" "GET"))
  (when *spotify-user*
    (message "Welcome, %s!" (spotify-current-user-name))))

(defun spotify-current-user-name ()
  "Returns the user's display name of the current Spotify session."
  (gethash 'display_name *spotify-user*))

(defun spotify-current-user-id ()
  "Returns the user's id of the current Spotify session."
  (gethash 'id *spotify-user*))

(defun spotify-get-search-tracks-items (json)
  "Returns track items from the given search results json."
  (gethash 'items (gethash 'tracks json)))

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

(defun spotify-get-item-uri (json)
  "Returns the uri from the given track/album/artist object."
  (gethash 'uri json))

(defun spotify-search (type query)
  "Searches artists, albums, tracks or playlists that match a keyword string,
depending on the `type' argument."
  (let ((escaped-query (url-hexify-string query)))
    (spotify-api-call
     (format "/search?q=%s&type=%s&limit=%d&market=from_token"
             escaped-query type spotify-api-search-limit) "GET")))

;;;
;;; Modes
;;;

(defun spotify-popularity-bar (popularity)
  "Returns the popularity indicator bar proportional to the given parameter,
which must be a number between 0 and 100."
  (let ((num-bars (truncate (/ popularity 10))))
    (concat (make-string num-bars ?\u25cf)
            (make-string (- 10 num-bars) ?\u25cb))))

(defvar spotify-remote-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "R") 'spotify-toggle-repeating)
    (define-key map (kbd "S") 'spotify-toggle-shuffling)
    map)
  "Local keymap for `spotify-remote-mode' buffers.")

(define-minor-mode spotify-remote-mode
  "Toggles Spotify Remote mode.
A positive prefix argument enables the mode, any other prefix
argument disables it. From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.
     
When Spotify Remote mode is enabled, it's possible to toggle
the repeating and shuffling status of the running Spotify process.
See commands \\[spotify-toggle-repeating] and
\\[spotify-toggle-shuffling]."
  :group 'spotify
  :init-value nil
  :lighter " >=")

(defvar spotify-track-search-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") 'spotify-track-select)
    (define-key map (kbd "M-RET") 'spotify-track-select-album)
    map)
  "Local keymap for `spotify-track-search-mode' buffers.")

(define-derived-mode spotify-track-search-mode tabulated-list-mode "Tracks"
  "Major mode for displaying the track listing returned by a Spotify search.")

(defun spotify-track-select ()
  "Plays the track under the cursor."
  (interactive)
  (spotify-play-track (car (tabulated-list-get-id))))

(defun spotify-track-select-album ()
  "Plays the album of the track under the cursor."
  (interactive)
  (spotify-play-track (cdr (tabulated-list-get-id))))

(defun spotify-track-search-print (songs)
  (let ((default-width (truncate (/ (- (window-width) 20) 3)))
        entries)
    (setq tabulated-list-format
          (vector '("#" 3 nil :right-align t)
                  `("Track Name" ,default-width t)
                  `("Artist" ,default-width t)
                  `("Album" ,default-width t)
                  '("Popularity" 10 t)))
    (dolist (song songs)
      (push (list (cons (spotify-get-item-uri song)
                        (spotify-get-item-uri (spotify-get-track-album song)))
                  (vector (number-to-string (spotify-get-track-number song))
                          (spotify-get-item-name song)
                          (spotify-get-track-artist song)
                          (spotify-get-track-album-name song)
                          (spotify-popularity-bar (spotify-get-track-popularity song))))
            entries))
    (setq tabulated-list-entries (nreverse entries))
    (tabulated-list-init-header)
    (tabulated-list-print)))

;;;###autoload
(defun spotify-track-search (query)
  "Searches for tracks that match the given query string."
  (interactive "sSpotify Search (Tracks): ")
  (let ((json (spotify-search 'track query))
	(buffer (get-buffer-create (format "*Spotify Search: %s*" query))))
    (pop-to-buffer buffer)
    (spotify-track-search-mode)
    (spotify-remote-mode)
    (spotify-track-search-print (spotify-get-search-tracks-items json))
    buffer))

(provide 'spotify)
