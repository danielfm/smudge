;; spotify-playlist-search.el --- Spotify.el playlist search major mode

;; Copyright (C) 2014 Daniel Fernandes Martins

;; Code:

(require 'spotify-api)

(defvar spotify-playlist-search-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET")   'spotify-playlist-select)
    (define-key map (kbd "M-RET") 'spotify-playlist-tracks)
    map)
  "Local keymap for `spotify-playlist-search-mode' buffers.")

;; Enables the `spotify-remote-mode' the track search buffer
(add-hook 'spotify-playlist-search-mode-hook 'spotify-remote-mode)

(define-derived-mode spotify-playlist-search-mode tabulated-list-mode "Playlist-Search"
  "Major mode for displaying the playlists returned by a Spotify search.")

(defun spotify-playlist-select ()
  "Plays the playlist under the cursor."
  (interactive)
  (spotify-play-track (first (tabulated-list-get-id))))

(defun spotify-playlist-tracks ()
  "Displays the tracks that belongs to the playlist under the cursor."
  (interactive)
  (let* ((selected-item-id (tabulated-list-get-id))
         (playlist-user-id (second selected-item-id))
         (playlist-name (third selected-item-id))
         (playlist-id (fourth selected-item-id)))
    (let ((json (spotify-api-playlist-tracks playlist-user-id playlist-id))
          (buffer (get-buffer-create (format "*Playlist Tracks: %s*" playlist-name))))
      (pop-to-buffer buffer)
      (spotify-track-search-mode)
      (spotify-track-search-print (spotify-get-playlist-tracks json)))))

(defun spotify-playlist-search-print (playlists)
  (let (entries)
    (setq tabulated-list-format
          (vector `("Playlist Name" ,(- (window-width) 45) t)
                  '("Owner Id" 30 t)
                  '("# Tracks" 8 nil :right-align t)))
    (dolist (playlist playlists)
      (let ((user-id (spotify-get-playlist-owner-id playlist))
            (playlist-name (spotify-get-item-name playlist)))
        (push (list (list (spotify-get-item-uri playlist)
                          user-id
                          playlist-name
                          (spotify-get-item-id playlist))
                    (vector playlist-name
                            user-id
                            (number-to-string (spotify-get-playlist-track-count playlist))))
              entries)))
    (setq tabulated-list-entries (nreverse entries))
    (tabulated-list-init-header)
    (tabulated-list-print)))

(provide 'spotify-playlist-search)
