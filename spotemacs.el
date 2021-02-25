;;; spotemacs.el --- control the Spotify app from Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018 Daniel Fernandes Martins

;; Keywords: multimedia, music, spotify, spotemacs
;; Package: spotemacs

;;; Commentary:

;; This mode requires at least GNU Emacs 24.4

;; Before using this mode, first go the Spotify Web API console
;; <https://developer.spotify.com/my-applications> and create a new application, adding
;; <http://localhost:8080/spotemacs-callback> as the redirect URI (or whichever port you have
;; specified via customize).

;; After requiring `spotemacs', make sure to define the client id and client secrets, along with some
;; other important settings.  See README.md for the complete list of settings and usage information.

;;; Code:

(when (version< emacs-version "24.4")
  (error "Spotemacs requires at least GNU Emacs 24.4"))

(require 'subr-x)
(require 'json)
(require 'tabulated-list)
(require 'easymenu)

(require 'spotemacs-api)
(require 'spotemacs-track)
(require 'spotemacs-playlist)
(require 'spotemacs-device-select)
(require 'spotemacs-controller)
(require 'spotemacs-remote)

(spotemacs-when-darwin    (require 'spotemacs-apple))
(spotemacs-when-gnu-linux (require 'spotemacs-dbus))

(require 'spotemacs-connect)

(defgroup spotemacs nil
  "Spotify client."
  :version "0.0.1"
  :group 'multimedia)

;;;###autoload
(defun spotemacs-track-search (query)
  "Search for tracks that match the given QUERY string."
  (interactive "sSpotify Search (Tracks): ")
  (let ((buffer (get-buffer-create (format "*Track Search: %s*" query))))
    (with-current-buffer buffer
      (spotemacs-track-search-mode)
      (spotemacs-track-search-update query 1))))

;;;###autoload
(defun spotemacs-playlist-search (query)
  "Search for playlists that match the given QUERY string."
  (interactive "sSpotify Search (Playlists): ")
  (let ((buffer (get-buffer-create (format "*Playlist Search: %s*" query))))
    (with-current-buffer buffer
      (spotemacs-playlist-search-mode)
      (spotemacs-playlist-search-update query 1))))

;;;###autoload
(defun spotemacs-recently-played ()
  "Display recently played tracks."
  (interactive)
  (let ((buffer (get-buffer-create "*Recently Played*")))
    (with-current-buffer buffer
      (spotemacs-track-search-mode)
      (spotemacs-track-recently-played-tracks-update 1))))

;;;###autoload
(defun spotemacs-my-playlists ()
  "Display the current user's playlists."
  (interactive)
  (spotemacs-api-current-user
   (lambda (user)
     (spotemacs-user-playlists (spotemacs-api-get-item-id user)))))

;;;###autoload
(defun spotemacs-user-playlists (user-id)
  "Display the public playlists of the given user with USER-ID."
  (interactive "sSpotify User ID: ")
  (let ((buffer (get-buffer-create (format "*Playlists: %s*" user-id))))
    (with-current-buffer buffer
      (spotemacs-playlist-search-mode)
      (spotemacs-playlist-user-playlists-update user-id 1))))

;;;###autoload
(defun spotemacs-featured-playlists ()
  "Display Spotify's featured playlists."
  (interactive)
  (let ((buffer (get-buffer-create "*Featured Playlists*")))
    (with-current-buffer buffer
      (spotemacs-playlist-search-mode)
      (spotemacs-playlist-featured-playlists-update 1))))

;;;###autoload
(defun spotemacs-create-playlist (name public)
  "Create an empty playlist owned by the current user.
Prompt for the NAME and whether it should be made PUBLIC."
  (interactive
   (list (read-string "Playlist name: ")
         (y-or-n-p "Make the playlist public? ")))
  (if (string= name "")
      (message "Playlist name not provided; aborting")
    (spotemacs-api-current-user
     (lambda (user)
       (spotemacs-api-playlist-create
        (spotemacs-api-get-item-id user)
        name
        public
        (lambda (new-playlist)
          (if new-playlist
              (message (format "Playlist '%s' created" (spotemacs-api-get-item-name new-playlist)))
            (message "Error creating the playlist"))))))))

;;;###autoload
(defun spotemacs-select-device ()
  "Allow for the selection of a device via Spotify Connect for transport functions."
  (interactive)
  (spotemacs-api-current-user
   (lambda (user)
     (if (not (string= (gethash 'product user) "premium"))
         (message "This feature requires a Spotify premium subscription.")
       (let ((buffer (get-buffer-create "*Devices*")))
         (with-current-buffer buffer
           (spotemacs-device-select-mode)
           (spotemacs-device-select-update)))))))

(defvar spotemacs-command-map
  (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-r") #'spotemacs-controller-toggle-repeat)
            (define-key map (kbd "M-s") #'spotemacs-controller-toggle-shuffle)
            (define-key map (kbd "M-p") #'spotemacs-controller-toggle-play)
            (define-key map (kbd "M-b") #'spotemacs-controller-previous-track)
            (define-key map (kbd "M-u") #'spotemacs-controller-volume-up)
            (define-key map (kbd "M-d") #'spotemacs-controller-volume-down)
            (define-key map (kbd "M-f") #'spotemacs-controller-next-track)
            (define-key map (kbd "p m") #'spotemacs-my-playlists)
            (define-key map (kbd "p f") #'spotemacs-featured-playlists)
            (define-key map (kbd "p u") #'spotemacs-user-playlists)
            (define-key map (kbd "p s") #'spotemacs-playlist-search)
            (define-key map (kbd "p c") #'spotemacs-create-playlist)
            (define-key map (kbd "t s") #'spotemacs-track-search)
            (define-key map (kbd "d") #'spotemacs-select-device)
            map)
  "Keymap for Spotify commands after 'spotemacs-keymap-prefix'.")
(fset 'spotemacs-command-map spotemacs-command-map)

(easy-menu-add-item nil '("Tools")
  '("Spotemacs"
    ["Play/Pause"     spotemacs-toggle-play    :active global-spotemacs-remote-mode]
    ["Previous Track" spotemacs-previous-track :active global-spotemacs-remote-mode]
    ["Next Track"     spotemacs-next-track     :active global-spotemacs-remote-mode]
    "--"
    ["Select Device"  spotemacs-select-device      :active global-spotemacs-remote-mode]
    ["Mute/Unmute"    spotemacs-volume-mute-unmute :active global-spotemacs-remote-mode]
    "--"
    ["Shuffle" spotemacs-toggle-shuffle :active global-spotemacs-remote-mode]
    ["Repeat"  spotemacs-toggle-repeat  :active global-spotemacs-remote-mode]
    "--"
    ["Search Tracks..."    spotemacs-track-search       :active global-spotemacs-remote-mode]
    ["Featured Playlists"  spotemacs-featured-playlists :active global-spotemacs-remote-mode]
    ["My Playlists"        spotemacs-my-playlists       :active global-spotemacs-remote-mode]
    ["User Playlists..."   spotemacs-user-playlists     :active global-spotemacs-remote-mode]
    ["Search Playlists..." spotemacs-playlist-search    :active global-spotemacs-remote-mode]
    ["Create Playlist..."  spotemacs-create-playlist    :active global-spotemacs-remote-mode]
    "--"
    ["Spotify Remote Mode" global-spotemacs-remote-mode :style toggle :selected global-spotemacs-remote-mode]))

(defun spotemacs-remote-popup-menu ()
  "Popup menu when in spotemacs-remote-mode."
  (interactive)
  (popup-menu
   '("Spotemacs"
     ["Play/Pause" spotemacs-toggle-play]
     ["Previous Track" spotemacs-previous-track]
     ["Next Track" spotemacs-next-track]
     "--"
     ["Select Device" spotemacs-select-device]
     ["Mute/Unmute" spotemacs-volume-mute-unmute]
     "--"
     ["Shuffle" spotemacs-toggle-shuffle]
     ["Repeat"  spotemacs-toggle-repeat]
     "--"
     ["Search Tracks..."    spotemacs-track-search]
     ["Featured Playlists"  spotemacs-featured-playlists]
     ["My Playlists"        spotemacs-my-playlists]
     ["User Playlists..."   spotemacs-user-playlists]
     ["Search Playlists..." spotemacs-playlist-search]
     ["Create Playlist..."  spotemacs-create-playlist])))

(provide 'spotemacs)
;;; spotemacs.el ends here
