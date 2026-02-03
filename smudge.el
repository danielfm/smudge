;;; smudge.el --- Control the Spotify app  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2025 Daniel Martins

;; Keywords: multimedia, music, spotify, smudge
;; Package: smudge
;; Package-Requires: ((emacs "27.1") (simple-httpd "1.5.1") (request "0.3") (oauth2 "0.18"))
;; Version: 1.0.0
;; Homepage: https://github.com/danielfm/smudge

;; SPDX-License-Identifier:  GPL-3.0-or-later

;;; Commentary:

;; This mode requires at least GNU Emacs 27.1

;; Before using this mode, first go the Spotify Web API console
;; <https://developer.spotify.com/my-applications> and create a new application,
;; adding <http://127.0.0.1:8080/smudge_api_callback> as the redirect URI (or
;; whichever port you have specified via customize).

;; After requiring `smudge', make sure to define the client id and client
;; secrets, along with some other important settings.

;;; Code:

(when (version< emacs-version "27.1")
  (error "Smudge requires at least GNU Emacs 27.1"))

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
(require 'smudge-lyrics)

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
(defun smudge-my-library ()
  "Display the songs saved in the current user's Liked Songs."
  (interactive)
  (let ((buffer (get-buffer-create "*Liked Songs*")))
    (with-current-buffer buffer
      (smudge-track-search-mode)
      (smudge-track-my-library-update 1))))

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

(defvar smudge-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") '("smudge/play-pause"     . smudge-controller-toggle-play))
    (define-key map (kbd "b")   '("smudge/previous-track" . smudge-controller-previous-track))
    (define-key map (kbd "n")   '("smudge/next-track"     . smudge-controller-next-track))
    (define-key map (kbd "d")   '("smudge/select-device"  . smudge-select-device))
    (define-key map (kbd "r")   '("smudge/toggle-repeat"  . smudge-controller-toggle-repeat))
    (define-key map (kbd "s")   '("smudge/toggle-shuffle" . smudge-controller-toggle-shuffle))
    (define-key map (kbd "p")   '("smudge/playlists"      . smudge-playlists))
    (define-key map (kbd "t")   '("smudge/tracks"         . smudge-tracks))
    (define-key map (kbd "l")   '("smudge/lyrics"         . smudge-lyrics-popup))
    (define-key map (kbd "v")   '("smudge/volume"         . smudge-volume))
    map)
  "Keymap for Spotify commands after \\='smudge-keymap-prefix\\='.")

;;;###autoload
(defalias 'smudge-playlists
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") '("smudge/my-playlists"     . smudge-my-playlists))
    (define-key map (kbd "u") '("smudge/user-playlists"   . smudge-user-playlists))
    (define-key map (kbd "s") '("smudge/search-playlists" . smudge-playlist-search))
    (define-key map (kbd "c") '("smudge/create-playlists" . smudge-create-playlist))
    map)
  "Playlist-related bindings.")

;;;###autoload
(defalias 'smudge-tracks
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") '("smudge/search-tracks"       . smudge-track-search))
    (define-key map (kbd "r") '("smudge/recently-played"     . smudge-recently-played))
    (define-key map (kbd "l") '("smudge/save-to-library"     . smudge-save-playing-track-to-library))
    (define-key map (kbd "k") '("smudge/remove-from-library" . smudge-remove-playing-track-from-library))
    map)
  "Track-related bindings.")

;;;###autoload
(defalias 'smudge-volume
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m")   '("smudge/mute-unmute" . smudge-controller-volume-mute-unmute))
    (define-key map (kbd "u")   '("smudge/volume-up"   . smudge-controller-volume-up))
    (define-key map (kbd "d")   '("smudge/volume-down" . smudge-controller-volume-down))
    map)
  "Spotify player volume bindings.")

(easy-menu-add-item nil '("Tools")
                    '("Smudge"
                      ["Play/Pause"     smudge-controller-toggle-play]
                      ["Previous Track" smudge-controller-previous-track]
                      ["Next Track"     smudge-controller-next-track]
                      ["Lyrics"         smudge-lyrics-popup]
                      "--"
                      ["Select Playing Device" smudge-select-device]
                      ["Mute/Unmute"           smudge-controller-volume-mute-unmute]
                      "--"
                      ["Shuffle" smudge-controller-toggle-shuffle]
                      ["Repeat"  smudge-controller-toggle-repeat]
                      "--"
                      ["Search Tracks..."    smudge-track-search]
                      ["My Playlists"        smudge-my-playlists]
                      ["User Playlists..."   smudge-user-playlists]
                      ["Search Playlists..." smudge-playlist-search]
                      ["Create Playlist..."  smudge-create-playlist]
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
     ["Lyrics" smudge-lyrics-popup]
     "--"
     ["Select Device" smudge-select-device]
     ["Mute/Unmute" smudge-controller-volume-mute-unmute]
     "--"
     ["Shuffle" smudge-controller-toggle-shuffle]
     ["Repeat"  smudge-controller-toggle-repeat]
     "--"
     ["Search Tracks..."    smudge-track-search]
     ["My Playlists"        smudge-my-playlists]
     ["User Playlists..."   smudge-user-playlists]
     ["Search Playlists..." smudge-playlist-search]
     ["Create Playlist..."  smudge-create-playlist])))

(provide 'smudge)
;;; smudge.el ends here
