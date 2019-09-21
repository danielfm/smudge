;;; package --- Summary

;;; Commentary:

;; spotify-device-select.el --- Spotify.el device selection major mode

;; Copyright (C) 2019 Jason Dufair

;;; Code:

(require 'spotify-api)

(defvar spotify-device-select-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "M-RET") 'spotify-device-select)
    (define-key map (kbd "l")     'spotify-playlist-load-more)
    (define-key map (kbd "g")     'spotify-playlist-reload)
    map)
  "Local keymap for `spotify-device-select-mode' buffers.")

(define-derived-mode spotify-device-select-mode tabulated-list-mode "Device-Select"
  "Major mode for selecting a Spotify Connect device for transport.")

(defun spotify-playlist-search-update (current-page)
  "Fetches the CURRENT-PAGE of devices using the device list endpoint."
  (let* ((json (spotify-api-search 'playlist spotify-query current-page))
         (items (spotify-get-search-playlist-items json)))
    (if items
        (progn
          (spotify-playlist-search-print items current-page)
          (message "playlist view updated"))
      (message "No more playlists"))))



(provide 'spotify-device-select)

;;; spotify-device-select.el ends here
