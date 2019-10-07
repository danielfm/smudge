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
(require 'spotify-device-select)
(require 'spotify-controller)
(require 'spotify-remote)

(when-darwin    (require 'spotify-apple))
(when-gnu-linux (require 'spotify-dbus))

(require 'spotify-connect)

(defgroup spotify nil
  "Spotify client."
  :version "0.0.1"
  :group 'multimedia)

;;;###autoload
(defun spotify-track-search (query)
  "Search for tracks that match the given query string."
  (interactive "sSpotify Search (Tracks): ")
  (let ((buffer (get-buffer-create (format "*Track Search: %s*" query))))
    (with-current-buffer buffer
      (spotify-track-search-mode)
      (spotify-track-search-update query 1))))

;;;###autoload
(defun spotify-playlist-search (query)
  "Search for playlists that match the given query string."
  (interactive "sSpotify Search (Playlists): ")
  (let ((buffer (get-buffer-create (format "*Playlist Search: %s*" query))))
    (with-current-buffer buffer
      (spotify-playlist-search-mode)
      (spotify-playlist-search-update query 1))))

;;;###autoload
(defun spotify-recently-played ()
  "Display recently played tracks."
  (interactive)
  (let ((buffer (get-buffer-create "*Recently Played*")))
    (with-current-buffer buffer
      (spotify-track-search-mode)
      (spotify-recently-played-tracks-update 1))))

;;;###autoload
(defun spotify-my-playlists ()
  "Display the current user's playlists."
  (interactive)
  (spotify-current-user
   (lambda (user)
     (spotify-user-playlists (spotify-get-item-id user)))))

;;;###autoload
(defun spotify-user-playlists (user-id)
  "Display the public playlists of the given user."
  (interactive "sSpotify User ID: ")
  (let ((buffer (get-buffer-create (format "*Playlists: %s*" user-id))))
    (with-current-buffer buffer
      (spotify-playlist-search-mode)
      (spotify-user-playlists-update user-id 1))))

;;;###autoload
(defun spotify-featured-playlists ()
  "Display Spotify's featured playlists."
  (interactive)
  (let ((buffer (get-buffer-create "*Featured Playlists*")))
    (with-current-buffer buffer
      (spotify-playlist-search-mode)
      (spotify-featured-playlists-update 1))))

;;;###autoload
(defun spotify-create-playlist (name is-public)
  "Create an empty playlist owned by the current user."
  (interactive
   (list (read-string "Playlist name: ")
         (y-or-n-p "Make the playlist public? ")))
  (lexical-let ((name name)
                (is-public is-public))
    (spotify-current-user
     (lambda (user)
       (spotify-api-playlist-create
        (spotify-get-item-id user)
        name
        is-public
        (lambda (new-playlist)
          (if new-playlist
              (message (format "Playlist '%s' created" (spotify-get-item-name new-playlist)))
            (message "Error creating the playlist"))))))))

;;;###autoload
(defun spotify-select-device ()
  "Allow for the selection of a device via Spotify Connect for transport functions."
  (interactive)
  (spotify-current-user
   (lambda (user)
     (if (not (string= (gethash 'product user) "premium"))
         (message "This feature requires a Spotify premium subscription.")
       (let ((buffer (get-buffer-create "*Devices*")))
         (with-current-buffer buffer
           (spotify-device-select-mode)
           (spotify-device-select-update)))))))

(provide 'spotify)
