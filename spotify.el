;;; spotify.el --- control the Spotify app from Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018 Daniel Fernandes Martins

;; Keywords: multimedia, music, spotify
;; Package: spotify

;;; Commentary:

;; This mode requires at least GNU Emacs 24.4

;; Before using this mode, first go the Spotify Web API console
;; <https://developer.spotify.com/my-applications> and create a new application, adding
;; <http://localhost:8080/spotify-callback> as the redirect URI (or whichever port you have
;; specified via customize).

;; After requiring `spotify', make sure to define the client id and client secrets, along with some
;; other important settings.  See README.md for the complete list of settings and usage information.

;;; Code:

(when (version< emacs-version "24.4")
  (error "Spotify requires at least GNU Emacs 24.4"))

(require 'subr-x)
(require 'json)
(require 'tabulated-list)
(require 'easymenu)

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
  "Search for tracks that match the given QUERY string."
  (interactive "sSpotify Search (Tracks): ")
  (let ((buffer (get-buffer-create (format "*Track Search: %s*" query))))
    (with-current-buffer buffer
      (spotify-track-search-mode)
      (spotify-track-search-update query 1))))

;;;###autoload
(defun spotify-playlist-search (query)
  "Search for playlists that match the given QUERY string."
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
  "Display the public playlists of the given user with USER-ID."
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
(defun spotify-create-playlist (name public)
  "Create an empty playlist owned by the current user.
Prompt for the NAME and whether it should be made PUBLIC."
  (interactive
   (list (read-string "Playlist name: ")
         (y-or-n-p "Make the playlist public? ")))
  (if (string= name "")
      (message "Playlist name not provided; aborting")
    (spotify-current-user
     (lambda (user)
       (spotify-api-playlist-create
        (spotify-get-item-id user)
        name
        public
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

(defvar spotify-command-map
  (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-r") #'spotify-toggle-repeat)
            (define-key map (kbd "M-s") #'spotify-toggle-shuffle)
            (define-key map (kbd "M-p") #'spotify-toggle-play)
            (define-key map (kbd "M-b") #'spotify-previous-track)
            (define-key map (kbd "M-u") #'spotify-volume-up)
            (define-key map (kbd "M-d") #'spotify-volume-down)
            (define-key map (kbd "M-f") #'spotify-next-track)
            (define-key map (kbd "p m") #'spotify-my-playlists)
            (define-key map (kbd "p f") #'spotify-featured-playlists)
            (define-key map (kbd "p u") #'spotify-user-playlists)
            (define-key map (kbd "p s") #'spotify-playlist-search)
            (define-key map (kbd "p c") #'spotify-create-playlist)
            (define-key map (kbd "t s") #'spotify-track-search)
            (define-key map (kbd "d") #'spotify-select-device)
            map)
  "Keymap for Spotify commands after 'spotify-keymap-prefix'.")
(fset 'spotify-command-map spotify-command-map)

(easy-menu-add-item nil '("Tools")
  '("Spotify"
    ["Play/Pause"     spotify-toggle-play    :active global-spotify-remote-mode]
    ["Previous Track" spotify-previous-track :active global-spotify-remote-mode]
    ["Next Track"     spotify-next-track     :active global-spotify-remote-mode]
    "--"
    ["Select Device"  spotify-select-device      :active global-spotify-remote-mode]
    ["Mute/Unmute"    spotify-volume-mute-unmute :active global-spotify-remote-mode]
    "--"
    ["Shuffle" spotify-toggle-shuffle :active global-spotify-remote-mode]
    ["Repeat"  spotify-toggle-repeat  :active global-spotify-remote-mode]
    "--"
    ["Search Tracks..."    spotify-track-search       :active global-spotify-remote-mode]
    ["Featured Playlists"  spotify-featured-playlists :active global-spotify-remote-mode]
    ["My Playlists"        spotify-my-playlists       :active global-spotify-remote-mode]
    ["User Playlists..."   spotify-user-playlists     :active global-spotify-remote-mode]
    ["Search Playlists..." spotify-playlist-search    :active global-spotify-remote-mode]
    ["Create Playlist..."  spotify-create-playlist    :active global-spotify-remote-mode]
    "--"
    ["Spotify Remote Mode" global-spotify-remote-mode :style toggle :selected global-spotify-remote-mode]))

(defun spotify-remote-popup-menu ()
  "Popup menu when in spotify-remote-mode."
  (interactive)
  (popup-menu
   '("Spotify"
     ["Play/Pause" spotify-toggle-play]
     ["Previous Track" spotify-previous-track]
     ["Next Track" spotify-next-track]
     "--"
     ["Select Device" spotify-select-device]
     ["Mute/Unmute" spotify-volume-mute-unmute]
     "--"
     ["Shuffle" spotify-toggle-shuffle]
     ["Repeat"  spotify-toggle-repeat]
     "--"
     ["Search Tracks..."    spotify-track-search]
     ["Featured Playlists"  spotify-featured-playlists]
     ["My Playlists"        spotify-my-playlists]
     ["User Playlists..."   spotify-user-playlists]
     ["Search Playlists..." spotify-playlist-search]
     ["Create Playlist..."  spotify-create-playlist])))

(provide 'spotify)
;;; spotify.el ends here
