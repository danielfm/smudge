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

(require 'spotify-api)
(require 'spotify-track-search)
(require 'spotify-playlist-search)
(require 'spotify-remote)
(require 'spotify-apple)

(when (version< emacs-version "24.4")
  (error "Spotify requires at least GNU Emacs 24.4"))

(defgroup spotify nil
  "Spotify client."
  :version "0.0.1"
  :group 'multimedia)

;;; TODO: D-Bus support not implemented
(defcustom spotify-transport 'apple
  "How the commands should be sent to Spotify process."
  :type '(choice (symbol :tag "AppleScript" apple)
                 (symbol :tag "D-Bus" dbus)))

;; simple facility to emulate multimethods
(defun spotify-apply (suffix &rest args)
  (let ((func-name (format "spotify-%s-%s" spotify-transport suffix)))
    (apply (intern func-name) args)))

(defun spotify-current-track-msg ()
  "Returns a string that describes the track being played in Spotify app."
  (let ((artist (spotify-current-track-artist))
        (name (spotify-current-track-name)))
    (if (spotify-playing-p)
        (message "Spotify is now playing: %s - %s" artist name)
      (message "Spotify is now paused"))))

(defun spotify-shuffling-status-msg ()
  "Returns a string that describes whether shuffling is enabled in Spotify app."
  (message "Spotify shuffling is %s"
           (if (spotify-shuffling-p) "on" "off")))

(defun spotify-repeating-status-msg ()
  "Returns a string that describes whether repeating is enabled in Spotify app."
  (message "Spotify repeating is %s"
           (if (spotify-repeating-p) "on" "off")))

(defun spotify-playing-status-msg ()
  "Returns a string that describes whether Spotify is paused or playing."
  (message "Spotify is now %s"
           (if (spotify-playing-p) "playing" "paused")))

(defun spotify-player-info ()
  "Returns a string that describes what's being played."
  (interactive)
  (spotify-current-track-msg))

(defun spotify-play-track (context-id)
  "Sends a `play' command to Spotify process passing a context id."
  (interactive)
  (spotify-apply "player-play-track" context-id)
  (run-at-time "1 sec" nil 'spotify-current-track-msg))

(defun spotify-toggle-play ()
  "Sends a `playpause' command to Spotify process."
  (interactive)
  (spotify-apply "player-toggle-play")
  (run-at-time "1 sec" nil 'spotify-playing-status-msg))

(defun spotify-play ()
  "Sends a `play' command to Spotify process."
  (interactive)
  (spotify-apply "player-play")
  (run-at-time "1 sec" nil 'spotify-playing-status-msg))

(defun spotify-next-track ()
  "Sends a `next track' command to Spotify process."
  (interactive)
  (spotify-apply "player-next-track")
  (run-at-time "1 sec" nil 'spotify-current-track-msg))

(defun spotify-previous-track ()
  "Sends a `previous track' command to Spotify process."
  (interactive)
  (spotify-apply "player-previous-track")
  (run-at-time "1 sec" nil 'spotify-current-track-msg))

(defun spotify-pause ()
  "Sends a `pause' command to Spotify process."
  (interactive)
  (spotify-apply "player-pause")
  (run-at-time "1 sec" nil 'spotify-playing-status-msg))

(defun spotify-playing-p ()
  "Returns whether Spotify is playing."
  (interactive)
  (spotify-apply "player-playing-p"))

(defun spotify-repeating-p ()
  "Returns whether Spotify have repeating turned on."
  (interactive)
  (spotify-apply "repeating-p"))

(defun spotify-toggle-repeat ()
  "Sends a command to Spotify process to toggle the repeating flag."
  (interactive)
  (spotify-apply "toggle-repeat")
  (run-at-time "1 sec" nil 'spotify-repeating-status-msg))

(defun spotify-shuffling-p ()
  "Returns whether Spotify have shuffling turned on."
  (interactive)
  (spotify-apply "shuffling-p"))

(defun spotify-toggle-shuffle ()
  "Sends a command to Spotify process to toggle the shuffling flag."
  (interactive)
  (spotify-apply "toggle-shuffle")
  (run-at-time "1 sec" nil 'spotify-shuffling-status-msg))

(defun spotify-current-track-artist ()
  "Retrieves the artist name of the track being played in Spotify app."
  (interactive)
  (spotify-apply "current-track-artist"))

(defun spotify-current-track-album ()
  "Retrieves the album name of the track being played in Spotify app."
  (interactive)
  (spotify-apply "current-track-album"))

(defun spotify-current-track-name ()
  "Retrieves the name of the track being played in Spotify app."
  (interactive)
  (spotify-apply "current-track-name"))

;;;###autoload
(defun spotify-track-search (query)
  "Searches for tracks that match the given query string."
  (interactive "sSpotify Search (Tracks): ")
  (let ((json (spotify-api-search 'track query))
	(buffer (get-buffer-create (format "*Track Search: %s*" query))))
    (pop-to-buffer buffer)
    (spotify-track-search-mode)
    (spotify-track-search-print (spotify-get-search-track-items json))
    buffer))

;;;###autoload
(defun spotify-playlist-search (query)
  "Searches for playlists that match the given query string."
  (interactive "sSpotify Search (Playlists): ")
  (let ((json (spotify-api-search 'playlist query))
        (buffer (get-buffer-create (format "*Playlist Search: %s*" query))))
    (pop-to-buffer buffer)
    (spotify-playlist-search-mode)
    (spotify-playlist-search-print (spotify-get-search-playlist-items json))))

;;;###autoload
(defun spotify-my-playlists ()
  "Displays the current user's playlists."
  (interactive)
  (let ((json (spotify-api-user-playlists (spotify-current-user-id)))
        (buffer (get-buffer-create "*My Playlists*")))
    (pop-to-buffer buffer)
    (spotify-playlist-search-mode)
    (spotify-playlist-search-print (spotify-get-items json))))

(provide 'spotify)
