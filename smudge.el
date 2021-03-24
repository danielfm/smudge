;;; smudge.el --- Control the Spotify app  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018 Daniel Fernandes Martins

;; Keywords: multimedia, music, spotify, smudge
;; Package: smudge
;; Package-Requires: ((emacs "27.1") (simple-httpd "1.5") (request "0.3") (oauth2 "0.16"))
;; Version: 1.0.0
;; Homepage: https://github.com/danielfm/smudge

;; SPDX-License-Identifier:  GPL-3.0-or-later

;;; Commentary:

;; This mode requires at least GNU Emacs 24.4

;; Before using this mode, first go the Spotify Web API console
;; <https://developer.spotify.com/my-applications> and create a new application, adding
;; <http://localhost:8080/smudge-callback> as the redirect URI (or whichever port you have
;; specified via customize).

;; After requiring `smudge', make sure to define the client id and client secrets, along with some
;; other important settings.  See README.md for the complete list of settings and usage information.

;;; Code:

(when (version< emacs-version "24.4")
  (error "Smudge requires at least GNU Emacs 24.4"))

(require 'subr-x)
(require 'json)
(require 'tabulated-list)
(require 'easymenu)

(require 'smudge-api)
(require 'smudge-track)
(require 'smudge-playlist)
(require 'smudge-device-select)
(require 'smudge-controller)
(require 'smudge-remote)

(smudge-when-darwin    (require 'smudge-apple))
(smudge-when-gnu-linux (require 'smudge-dbus))

(require 'smudge-connect)

(defgroup smudge nil
  "Smudge Spotify client."
  :version "0.0.1"
  :group 'multimedia)

;;;###autoload
(defun smudge-track-search (query)
  "Search for tracks that match the given QUERY string."
  (interactive "sSpotify Search (Tracks): ")
  (let ((buffer (get-buffer-create (format "*Track Search: %s*" query))))
    (with-current-buffer buffer
      (smudge-track-search-mode)
      (smudge-track-search-update query 1))))

;;;###autoload
(defun smudge-playlist-search (query)
  "Search for playlists that match the given QUERY string."
  (interactive "sSpotify Search (Playlists): ")
  (let ((buffer (get-buffer-create (format "*Playlist Search: %s*" query))))
    (with-current-buffer buffer
      (smudge-playlist-search-mode)
      (smudge-playlist-search-update query 1))))

;;;###autoload
(defun smudge-recently-played ()
  "Display recently played tracks."
  (interactive)
  (let ((buffer (get-buffer-create "*Recently Played*")))
    (with-current-buffer buffer
      (smudge-track-search-mode)
      (smudge-track-recently-played-tracks-update 1))))

;;;###autoload
(defun smudge-my-playlists ()
  "Display the current user's playlists."
  (interactive)
  (smudge-api-current-user
   (lambda (user)
     (smudge-user-playlists (smudge-api-get-item-id user)))))

;;;###autoload
(defun smudge-user-playlists (user-id)
  "Display the public playlists of the given user with USER-ID."
  (interactive "sSpotify User ID: ")
  (let ((buffer (get-buffer-create (format "*Playlists: %s*" user-id))))
    (with-current-buffer buffer
      (smudge-playlist-search-mode)
      (smudge-playlist-user-playlists-update user-id 1))))

;;;###autoload
(defun smudge-featured-playlists ()
  "Display Spotify's featured playlists."
  (interactive)
  (let ((buffer (get-buffer-create "*Featured Playlists*")))
    (with-current-buffer buffer
      (smudge-playlist-search-mode)
      (smudge-playlist-featured-playlists-update 1))))

;;;###autoload
(defun smudge-create-playlist (name public)
  "Create an empty playlist owned by the current user.
Prompt for the NAME and whether it should be made PUBLIC."
  (interactive
   (list (read-string "Playlist name: ")
         (y-or-n-p "Make the playlist public? ")))
  (if (string= name "")
      (message "Playlist name not provided; aborting")
    (smudge-api-current-user
     (lambda (user)
       (smudge-api-playlist-create
        (smudge-api-get-item-id user)
        name
        public
        (lambda (new-playlist)
          (if new-playlist
              (message "Playlist '%s' created" (smudge-api-get-item-name new-playlist))
            (message "Error creating the playlist"))))))))

;;;###autoload
(defun smudge-select-device ()
  "Allow for the selection of a device via Spotify Connect for transport functions."
  (interactive)
  (smudge-api-current-user
   (lambda (user)
     (if (not (string= (gethash 'product user) "premium"))
         (message "This feature requires a Spotify premium subscription.")
       (let ((buffer (get-buffer-create "*Devices*")))
         (with-current-buffer buffer
           (smudge-device-select-mode)
           (smudge-device-select-update)))))))

(defvar smudge-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-r") #'smudge-controller-toggle-repeat)
    (define-key map (kbd "M-s") #'smudge-controller-toggle-shuffle)
    (define-key map (kbd "M-p") #'smudge-controller-toggle-play)
    (define-key map (kbd "M-b") #'smudge-controller-previous-track)
    (define-key map (kbd "M-u") #'smudge-controller-volume-up)
    (define-key map (kbd "M-d") #'smudge-controller-volume-down)
    (define-key map (kbd "M-f") #'smudge-controller-next-track)
    (define-key map (kbd "p m") #'smudge-my-playlists)
    (define-key map (kbd "p f") #'smudge-featured-playlists)
    (define-key map (kbd "p u") #'smudge-user-playlists)
    (define-key map (kbd "p s") #'smudge-playlist-search)
    (define-key map (kbd "p c") #'smudge-create-playlist)
    (define-key map (kbd "t s") #'smudge-track-search)
    (define-key map (kbd "d") #'smudge-select-device)
    map)
  "Keymap for Spotify commands after 'smudge-keymap-prefix'.")
(fset 'smudge-command-map smudge-command-map)

(easy-menu-add-item nil '("Tools")
                    '("Smudge"
                      ["Play/Pause"     smudge-controller-toggle-play    :active global-smudge-remote-mode]
                      ["Previous Track" smudge-controller-previous-track :active global-smudge-remote-mode]
                      ["Next Track"     smudge-controller-next-track     :active global-smudge-remote-mode]
                      "--"
                      ["Select Device"  smudge-select-device      :active global-smudge-remote-mode]
                      ["Mute/Unmute"    smudge-controller-volume-mute-unmute :active global-smudge-remote-mode]
                      "--"
                      ["Shuffle" smudge-controller-toggle-shuffle :active global-smudge-remote-mode]
                      ["Repeat"  smudge-controller-toggle-repeat  :active global-smudge-remote-mode]
                      "--"
                      ["Search Tracks..."    smudge-track-search       :active global-smudge-remote-mode]
                      ["Featured Playlists"  smudge-featured-playlists :active global-smudge-remote-mode]
                      ["My Playlists"        smudge-my-playlists       :active global-smudge-remote-mode]
                      ["User Playlists..."   smudge-user-playlists     :active global-smudge-remote-mode]
                      ["Search Playlists..." smudge-playlist-search    :active global-smudge-remote-mode]
                      ["Create Playlist..."  smudge-create-playlist    :active global-smudge-remote-mode]
                      "--"
                      ["Smudge Remote Mode" global-smudge-remote-mode :style toggle :selected global-smudge-remote-mode]))

(defun smudge-remote-popup-menu ()
  "Popup menu when in smudge-remote-mode."
  (interactive)
  (popup-menu
   '("Smudge"
     ["Play/Pause" smudge-controller-toggle-play]
     ["Previous Track" smudge-controller-previous-track]
     ["Next Track" smudge-controller-next-track]
     "--"
     ["Select Device" smudge-select-device]
     ["Mute/Unmute" smudge-controller-volume-mute-unmute]
     "--"
     ["Shuffle" smudge-controller-toggle-shuffle]
     ["Repeat"  smudge-controller-toggle-repeat]
     "--"
     ["Search Tracks..."    smudge-track-search]
     ["Featured Playlists"  smudge-featured-playlists]
     ["My Playlists"        smudge-my-playlists]
     ["User Playlists..."   smudge-user-playlists]
     ["Search Playlists..." smudge-playlist-search]
     ["Create Playlist..."  smudge-create-playlist])))

(provide 'smudge)
;;; smudge.el ends here
