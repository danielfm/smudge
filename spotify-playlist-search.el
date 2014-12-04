;; spotify-remote.el --- Spotify.el playlist search major mode

;; Copyright (C) 2014 Daniel Fernandes Martins

;; Code:

(defvar spotify-playlist-search-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") 'spotify-playlist-select)
    map)
  "Local keymap for `spotify-playlist-search-mode' buffers.")

;; Enables the `spotify-remote-mode' the track search buffer
(add-hook 'spotify-playlist-search-mode-hook 'spotify-remote-mode)

(define-derived-mode spotify-playlist-search-mode tabulated-list-mode "Playlist-Search"
  "Major mode for displaying the playlists returned by a Spotify search.")

(defun spotify-playlist-select ()
  "Plays the playlist under the cursor."
  (interactive)
  (spotify-play-track (tabulated-list-get-id)))

(defun spotify-playlist-search-print (playlists)
  (let (entries)
    (setq tabulated-list-format
          (vector `("Playlist Name" ,(- (window-width) 45) t)
                  '("Owner Id" 30 t)
                  '("# Tracks" 8 nil :right-align t)))
    (dolist (playlist playlists)
      (push (list (spotify-get-item-uri playlist)
                  (vector (spotify-get-item-name playlist)
                          (spotify-get-playlist-owner-id playlist)
                          (number-to-string (spotify-get-playlist-track-count playlist))))
            entries))
    (setq tabulated-list-entries (nreverse entries))
    (tabulated-list-init-header)
    (tabulated-list-print)))

(provide 'spotify-playlist-search)
