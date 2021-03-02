;;; spotify-client.el --- control the Spotify app from Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018 Daniel Fernandes Martins

;; Keywords: multimedia, music, spotify, spotify-client
;; Package: spotify-client
;; Package-Requires: ((emacs "24.4") (simple-httpd "1.5") (oauth2 "0.14") (request "0.3"))

;;; Commentary:

;; This mode requires at least GNU Emacs 24.4

;; Before using this mode, first go the Spotify Web API console
;; <https://developer.spotify.com/my-applications> and create a new application, adding
;; <http://localhost:8080/spotify-client-callback> as the redirect URI (or whichever port you have
;; specified via customize).

;; After requiring `spotify-client', make sure to define the client id and client secrets, along with some
;; other important settings.  See README.md for the complete list of settings and usage information.

;;; Code:

(when (version< emacs-version "24.4")
  (error "Spotify-Client requires at least GNU Emacs 24.4"))

(require 'subr-x)
(require 'json)
(require 'tabulated-list)
(require 'easymenu)

(require 'spotify-client-api)
(require 'spotify-client-track)
(require 'spotify-client-playlist)
(require 'spotify-client-device-select)
(require 'spotify-client-controller)
(require 'spotify-client-remote)

(spotify-client-when-darwin    (require 'spotify-client-apple))
(spotify-client-when-gnu-linux (require 'spotify-client-dbus))

(require 'spotify-client-connect)

(defgroup spotify-client nil
  "Spotify client."
  :version "0.0.1"
  :group 'multimedia)

;;;###autoload
(defun spotify-client-track-search (query)
  "Search for tracks that match the given QUERY string."
  (interactive "sSpotify Search (Tracks): ")
  (let ((buffer (get-buffer-create (format "*Track Search: %s*" query))))
    (with-current-buffer buffer
      (spotify-client-track-search-mode)
      (spotify-client-track-search-update query 1))))

;;;###autoload
(defun spotify-client-playlist-search (query)
  "Search for playlists that match the given QUERY string."
  (interactive "sSpotify Search (Playlists): ")
  (let ((buffer (get-buffer-create (format "*Playlist Search: %s*" query))))
    (with-current-buffer buffer
      (spotify-client-playlist-search-mode)
      (spotify-client-playlist-search-update query 1))))

;;;###autoload
(defun spotify-client-recently-played ()
  "Display recently played tracks."
  (interactive)
  (let ((buffer (get-buffer-create "*Recently Played*")))
    (with-current-buffer buffer
      (spotify-client-track-search-mode)
      (spotify-client-track-recently-played-tracks-update 1))))

;;;###autoload
(defun spotify-client-my-playlists ()
  "Display the current user's playlists."
  (interactive)
  (spotify-client-api-current-user
   (lambda (user)
     (spotify-client-user-playlists (spotify-client-api-get-item-id user)))))

;;;###autoload
(defun spotify-client-user-playlists (user-id)
  "Display the public playlists of the given user with USER-ID."
  (interactive "sSpotify User ID: ")
  (let ((buffer (get-buffer-create (format "*Playlists: %s*" user-id))))
    (with-current-buffer buffer
      (spotify-client-playlist-search-mode)
      (spotify-client-playlist-user-playlists-update user-id 1))))

;;;###autoload
(defun spotify-client-featured-playlists ()
  "Display Spotify's featured playlists."
  (interactive)
  (let ((buffer (get-buffer-create "*Featured Playlists*")))
    (with-current-buffer buffer
      (spotify-client-playlist-search-mode)
      (spotify-client-playlist-featured-playlists-update 1))))

;;;###autoload
(defun spotify-client-create-playlist (name public)
  "Create an empty playlist owned by the current user.
Prompt for the NAME and whether it should be made PUBLIC."
  (interactive
   (list (read-string "Playlist name: ")
         (y-or-n-p "Make the playlist public? ")))
  (if (string= name "")
      (message "Playlist name not provided; aborting")
    (spotify-client-api-current-user
     (lambda (user)
       (spotify-client-api-playlist-create
        (spotify-client-api-get-item-id user)
        name
        public
        (lambda (new-playlist)
          (if new-playlist
              (message (format "Playlist '%s' created" (spotify-client-api-get-item-name new-playlist)))
            (message "Error creating the playlist"))))))))

;;;###autoload
(defun spotify-client-select-device ()
  "Allow for the selection of a device via Spotify Connect for transport functions."
  (interactive)
  (spotify-client-api-current-user
   (lambda (user)
     (if (not (string= (gethash 'product user) "premium"))
         (message "This feature requires a Spotify premium subscription.")
       (let ((buffer (get-buffer-create "*Devices*")))
         (with-current-buffer buffer
           (spotify-client-device-select-mode)
           (spotify-client-device-select-update)))))))

(defvar spotify-client-command-map
  (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-r") #'spotify-client-controller-toggle-repeat)
            (define-key map (kbd "M-s") #'spotify-client-controller-toggle-shuffle)
            (define-key map (kbd "M-p") #'spotify-client-controller-toggle-play)
            (define-key map (kbd "M-b") #'spotify-client-controller-previous-track)
            (define-key map (kbd "M-u") #'spotify-client-controller-volume-up)
            (define-key map (kbd "M-d") #'spotify-client-controller-volume-down)
            (define-key map (kbd "M-f") #'spotify-client-controller-next-track)
            (define-key map (kbd "p m") #'spotify-client-my-playlists)
            (define-key map (kbd "p f") #'spotify-client-featured-playlists)
            (define-key map (kbd "p u") #'spotify-client-user-playlists)
            (define-key map (kbd "p s") #'spotify-client-playlist-search)
            (define-key map (kbd "p c") #'spotify-client-create-playlist)
            (define-key map (kbd "t s") #'spotify-client-track-search)
            (define-key map (kbd "d") #'spotify-client-select-device)
            map)
  "Keymap for Spotify commands after 'spotify-client-keymap-prefix'.")
(fset 'spotify-client-command-map spotify-client-command-map)

(easy-menu-add-item nil '("Tools")
  '("Spotify-Client"
    ["Play/Pause"     spotify-client-toggle-play    :active global-spotify-client-remote-mode]
    ["Previous Track" spotify-client-previous-track :active global-spotify-client-remote-mode]
    ["Next Track"     spotify-client-next-track     :active global-spotify-client-remote-mode]
    "--"
    ["Select Device"  spotify-client-select-device      :active global-spotify-client-remote-mode]
    ["Mute/Unmute"    spotify-client-volume-mute-unmute :active global-spotify-client-remote-mode]
    "--"
    ["Shuffle" spotify-client-toggle-shuffle :active global-spotify-client-remote-mode]
    ["Repeat"  spotify-client-toggle-repeat  :active global-spotify-client-remote-mode]
    "--"
    ["Search Tracks..."    spotify-client-track-search       :active global-spotify-client-remote-mode]
    ["Featured Playlists"  spotify-client-featured-playlists :active global-spotify-client-remote-mode]
    ["My Playlists"        spotify-client-my-playlists       :active global-spotify-client-remote-mode]
    ["User Playlists..."   spotify-client-user-playlists     :active global-spotify-client-remote-mode]
    ["Search Playlists..." spotify-client-playlist-search    :active global-spotify-client-remote-mode]
    ["Create Playlist..."  spotify-client-create-playlist    :active global-spotify-client-remote-mode]
    "--"
    ["Spotify Remote Mode" global-spotify-client-remote-mode :style toggle :selected global-spotify-client-remote-mode]))

(defun spotify-client-remote-popup-menu ()
  "Popup menu when in spotify-client-remote-mode."
  (interactive)
  (popup-menu
   '("Spotify-Client"
     ["Play/Pause" spotify-client-toggle-play]
     ["Previous Track" spotify-client-previous-track]
     ["Next Track" spotify-client-next-track]
     "--"
     ["Select Device" spotify-client-select-device]
     ["Mute/Unmute" spotify-client-volume-mute-unmute]
     "--"
     ["Shuffle" spotify-client-toggle-shuffle]
     ["Repeat"  spotify-client-toggle-repeat]
     "--"
     ["Search Tracks..."    spotify-client-track-search]
     ["Featured Playlists"  spotify-client-featured-playlists]
     ["My Playlists"        spotify-client-my-playlists]
     ["User Playlists..."   spotify-client-user-playlists]
     ["Search Playlists..." spotify-client-playlist-search]
     ["Create Playlist..."  spotify-client-create-playlist])))

(provide 'spotify-client)
;;; spotify-client.el ends here
