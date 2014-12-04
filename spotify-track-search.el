;; spotify-track-search.el --- Spotify.el track search major mode

;; Copyright (C) 2014 Daniel Fernandes Martins

;; Code:

(defvar spotify-track-search-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET")   'spotify-track-select)
    (define-key map (kbd "M-RET") 'spotify-track-select-album)
    map)
  "Local keymap for `spotify-track-search-mode' buffers.")

;; Enables the `spotify-remote-mode' the track search buffer
(add-hook 'spotify-track-search-mode-hook 'spotify-remote-mode)

(define-derived-mode spotify-track-search-mode tabulated-list-mode "Track-Search"
  "Major mode for displaying the track listing returned by a Spotify search.")

(defun spotify-track-select ()
  "Plays the track under the cursor."
  (interactive)
  (spotify-play-track (car (tabulated-list-get-id))))

(defun spotify-track-select-album ()
  "Plays the album of the track under the cursor."
  (interactive)
  (spotify-play-track (cdr (tabulated-list-get-id))))

(defun spotify-track-search-print (songs)
  (let ((default-width (truncate (/ (- (window-width) 20) 3)))
        entries)
    (setq tabulated-list-format
          (vector '("#" 3 nil :right-align t)
                  `("Track Name" ,default-width t)
                  `("Artist" ,default-width t)
                  `("Album" ,default-width t)
                  '("Popularity" 10 t)))
    (dolist (song songs)
      (push (list (cons (spotify-get-item-uri song)
                        (spotify-get-item-uri (spotify-get-track-album song)))
                  (vector (number-to-string (spotify-get-track-number song))
                          (spotify-get-item-name song)
                          (spotify-get-track-artist song)
                          (spotify-get-track-album-name song)
                          (spotify-popularity-bar (spotify-get-track-popularity song))))
            entries))
    (setq tabulated-list-entries (nreverse entries))
    (tabulated-list-init-header)
    (tabulated-list-print)))

(provide 'spotify-track-search)
