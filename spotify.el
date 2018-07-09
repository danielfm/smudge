;;; spotify.el --- control the Spotify app from Emacs

;; Copyright (C) 2014-2018 Daniel Fernandes Martins

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
;;  '(spotify-oauth2-client-secret "client-secret"))
;;
;; See 'README.md' for usage information.

;; Code:

(when (version< emacs-version "24.4")
  (error "Spotify requires at least GNU Emacs 24.4"))

(require 'json)
(require 'oauth2)
(require 'tabulated-list)

(require 'spotify-api)
(require 'spotify-track-search)
(require 'spotify-playlist-search)
(require 'spotify-controller)
(require 'spotify-remote)

(when-darwin    (require 'spotify-apple))
(when-gnu-linux (require 'spotify-dbus))

(defgroup spotify nil
  "Spotify client."
  :version "0.0.1"
  :group 'multimedia)

;;;###autoload
(defun spotify-track-search (query)
  "Searches for tracks that match the given query string."
  (interactive "sSpotify Search (Tracks): ")
  (let ((buffer (get-buffer-create (format "*Track Search: %s*" query))))
    (with-current-buffer buffer
      (spotify-track-search-mode)
      (spotify-track-search-set-list-format)
      (setq-local spotify-query query)
      (setq-local spotify-current-page 1)
      (setq tabulated-list-entries nil)
      (pop-to-buffer buffer)
      (spotify-track-search-update 1)
      buffer)))

;;;###autoload
(defun spotify-playlist-search (query)
  "Searches for playlists that match the given query string."
  (interactive "sSpotify Search (Playlists): ")
  (let ((buffer (get-buffer-create (format "*Playlist Search: %s*" query))))
    (with-current-buffer buffer
      (spotify-playlist-search-mode)
      (setq-local spotify-query query)
      (setq-local spotify-current-page 1)
      (setq tabulated-list-entries nil)
      (pop-to-buffer buffer)
      (spotify-playlist-search-update 1)
      buffer)))

;;;###autoload
(defun spotify-my-playlists ()
  "Displays the current user's playlists."
  (interactive)
  (spotify-user-playlists (spotify-current-user-id)))

;;;###autoload
(defun spotify-user-playlists (user-id)
  "Displays the public playlists of the given user."
  (interactive "sSpotify User ID: ")
  (let ((buffer (get-buffer-create (format "*Playlists: %s*" user-id))))
    (with-current-buffer buffer
      (spotify-playlist-search-mode)
      (setq-local spotify-user-id user-id)
      (setq-local spotify-current-page 1)
      (setq tabulated-list-entries nil)
      (pop-to-buffer buffer)
      (spotify-user-playlists-update user-id 1)
      buffer)))

;;;###autoload
(defun spotify-featured-playlists ()
  "Displays Spotify's featured playlists."
  (interactive)
  (let ((buffer (get-buffer-create "*Featured Playlists*")))
    (with-current-buffer buffer
      (spotify-playlist-search-mode)
      (setq-local spotify-current-page 1)
      (setq tabulated-list-entries nil)
      (pop-to-buffer buffer)
      (spotify-featured-playlists-update 1)
      buffer)))

;;;###autoload
(defun spotify-create-playlist (name is-public)
  "Creates an empty playlist owned by the current user."
  (interactive
   (list (read-string "Playlist name: ")
         (y-or-n-p "Make the playlist public? ")))
  (let ((new-playlist (spotify-api-playlist-create (spotify-current-user-id) name is-public)))
    (message (format "Playlist '%s' created" (spotify-get-item-name new-playlist)))))

(provide 'spotify)
