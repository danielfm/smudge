;; spotify-playlist-search.el --- Spotify.el playlist search major mode

;; Copyright (C) 2014 Daniel Fernandes Martins

;; Code:

(require 'spotify-api)

(defvar spotify-playlist-search-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET")   'spotify-playlist-select)
    (define-key map (kbd "M-RET") 'spotify-playlist-tracks)
    (define-key map (kbd "l")     'spotify-playlist-load-more)
    (define-key map (kbd "f")     'spotify-playlist-follow)
    (define-key map (kbd "u")     'spotify-playlist-unfollow)
    map)
  "Local keymap for `spotify-playlist-search-mode' buffers.")

;; Enables the `spotify-remote-mode' the track search buffer
(add-hook 'spotify-playlist-search-mode-hook 'spotify-remote-mode)

(define-derived-mode spotify-playlist-search-mode tabulated-list-mode "Playlist-Search"
  "Major mode for displaying the playlists returned by a Spotify search.")

(defun spotify-playlist-select ()
  "Plays the playlist under the cursor."
  (interactive)
  (spotify-play-track (first (tabulated-list-get-id)) ""))

(defun spotify-playlist-load-more ()
  "Loads the next page of results for the current playlist view."
  (interactive)
  (if (boundp 'spotify-query)
      (spotify-playlist-search-update (1+ spotify-current-page))
    (spotify-my-playlists-update (1+ spotify-current-page))))

(defun spotify-playlist-follow ()
  "Adds the current user as the follower of the playlist under the cursor."
  (interactive)
  (let* ((id-selected (tabulated-list-get-id))
         (owner-user-id (second id-selected))
         (playlist-name (third id-selected))
         (playlist-id (fourth id-selected)))
    (when (and (y-or-n-p (format "Follow playlist '%s'?" playlist-name))
               (spotify-api-playlist-follow owner-user-id playlist-id))
      (message (format "Followed playlist '%s'" playlist-name)))))

(defun spotify-playlist-unfollow ()
  "Removes the current user as the follower of the playlist under the cursor."
  (interactive)
  (let* ((id-selected (tabulated-list-get-id))
         (owner-user-id (second id-selected))
         (playlist-name (third id-selected))
         (playlist-id (fourth id-selected)))
    (when (and (y-or-n-p (format "Unfollow playlist '%s'?" playlist-name))
               (spotify-api-playlist-unfollow owner-user-id playlist-id))
      (message (format "Unfollow playlist '%s'" playlist-name)))))

(defun spotify-playlist-search-update (current-page)
  "Fetches the given page of results using the search endpoint."
  (let* ((json (spotify-api-search 'playlist spotify-query current-page))
         (items (spotify-get-search-playlist-items json)))
    (if items
        (progn
          (spotify-playlist-search-print items)
          (setq-local spotify-current-page current-page)
          (message "playlist view updated"))
      (message "No more playlists"))))

(defun spotify-my-playlists-update (current-page)
  "Fetches the given page of results using the user's playlist endpoint."
  (let* ((json (spotify-api-user-playlists (spotify-current-user-id) current-page))
         (items (spotify-get-items json)))
    (if items
        (progn
          (spotify-playlist-search-print items)
          (setq-local spotify-current-page current-page)
          (message "Playlist view updated"))
      (message "No more playlists"))))

(defun spotify-playlist-tracks ()
  "Displays the tracks that belongs to the playlist under the cursor."
  (interactive)
  (let* ((selected-item-id (tabulated-list-get-id))
         (playlist-uri (first selected-item-id))
         (playlist-user-id (second selected-item-id))
         (playlist-name (third selected-item-id))
         (playlist-id (fourth selected-item-id)))
    (let ((buffer (get-buffer-create (format "*Playlist Tracks: %s*" playlist-name))))
      (with-current-buffer buffer
        (spotify-track-search-mode)
        (spotify-track-search-set-list-format)
        (setq-local spotify-playlist-name playlist-name)
        (setq-local spotify-playlist-id playlist-id)
        (setq-local spotify-playlist-uri playlist-uri)
        (setq-local spotify-playlist-user-id playlist-user-id)
        (setq-local spotify-current-page 1)
        (setq tabulated-list-entries nil)
        (spotify-playlist-tracks-update 1)
        (pop-to-buffer buffer)
        buffer))))

(defun spotify-playlist-set-list-format ()
  "Configures the column data for the typical playlist view."
  (setq tabulated-list-format
        (vector `("Playlist Name" ,(- (window-width) 45) t)
                '("Owner Id" 30 t)
                '("# Tracks" 8 nil :right-align t))))

(defun spotify-playlist-search-print (playlists)
  "Appends the given playlists to the current playlist view."
  (let (entries)
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
    (setq tabulated-list-entries (append tabulated-list-entries (nreverse entries)))
    (tabulated-list-init-header)
    (tabulated-list-print t)))

(provide 'spotify-playlist-search)
