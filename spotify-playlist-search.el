;; spotify-playlist-search.el --- Spotify.el playlist search major mode

;; Copyright (C) 2014-2018 Daniel Fernandes Martins

;; Code:

(require 'spotify-api)

(defvar spotify-playlist-search-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "M-RET") 'spotify-playlist-select)
    (define-key map (kbd "l")     'spotify-playlist-load-more)
    (define-key map (kbd "g")     'spotify-playlist-reload)
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
  (let ((selected-playlist (tabulated-list-get-id)))
    (spotify-play-track nil selected-playlist)))

(defun spotify-playlist-reload ()
  "Reloads the first page of results for the current playlist view."
  (interactive)
  (let ((page 1))
    (cond ((bound-and-true-p spotify-query)          (spotify-playlist-search-update spotify-query page))
          ((bound-and-true-p spotify-browse-message) (spotify-featured-playlists-update page))
          (t                                         (spotify-user-playlists-update spotify-user-id page)))))

(defun spotify-playlist-load-more ()
  "Loads the next page of results for the current playlist view."
  (interactive)
  (let ((next-page (1+ spotify-current-page)))
    (cond ((bound-and-true-p spotify-query)          (spotify-playlist-search-update spotify-query next-page))
          ((bound-and-true-p spotify-browse-message) (spotify-featured-playlists-update next-page))
          (t                                         (spotify-user-playlists-update spotify-user-id next-page)))))

(defun spotify-playlist-follow ()
  "Adds the current user as the follower of the playlist under the cursor."
  (interactive)
  (lexical-let* ((selected-playlist (tabulated-list-get-id))
                 (name (spotify-get-item-name selected-playlist)))
    (when (y-or-n-p (format "Follow playlist '%s'?" name))
      (spotify-api-playlist-follow
       selected-playlist
       (lambda (_)
         (message (format "Followed playlist '%s'" name)))))))

(defun spotify-playlist-unfollow ()
  "Removes the current user as the follower of the playlist under the cursor."
  (interactive)
  (lexical-let* ((selected-playlist (tabulated-list-get-id))
                 (name (spotify-get-item-name selected-playlist)))
    (when (y-or-n-p (format "Unfollow playlist '%s'?" name))
      (spotify-api-playlist-unfollow
       selected-playlist
       (lambda (_)
         (message (format "Unfollowed playlist '%s'" name)))))))

(defun spotify-playlist-search-update (query current-page)
  "Fetches the given page of results using the search endpoint."
  (lexical-let ((current-page current-page)
                (query query)
                (buffer (current-buffer)))
    (spotify-api-search
     'playlist
     query
     current-page
     (lambda (playlists)
       (if-let ((items (spotify-get-search-playlist-items playlists)))
           (with-current-buffer buffer
             (setq-local spotify-current-page current-page)
             (setq-local spotify-query query)
             (pop-to-buffer buffer)
             (spotify-playlist-search-print items current-page)
             (message "Playlist view updated"))
         (message "No more playlists"))))))

(defun spotify-user-playlists-update (user-id current-page)
  "Fetches the given page of results using the user's playlist endpoint."
  (lexical-let ((user-id user-id)
                (current-page current-page)
                (buffer (current-buffer)))
    (spotify-api-user-playlists
     user-id
     current-page
     (lambda (playlists)
       (if-let ((items (spotify-get-items playlists)))
         (with-current-buffer buffer
           (setq-local spotify-user-id user-id)
           (setq-local spotify-current-page current-page)
           (pop-to-buffer buffer)
           (spotify-playlist-search-print items current-page)
           (message "Playlist view updated"))
         (message "No more playlists"))))))

(defun spotify-featured-playlists-update (current-page)
  "Fetches the given page of results using of Spotify's featured playlists"
  (lexical-let ((current-page current-page)
                (buffer (current-buffer)))
    (spotify-api-featured-playlists
     current-page
     (lambda (json)
       (if-let ((items (spotify-get-search-playlist-items json))
                (msg (spotify-get-message json)))
         (with-current-buffer buffer
           (setq-local spotify-current-page current-page)
           (setq-local spotify-browse-message msg)
           (pop-to-buffer buffer)
           (spotify-playlist-search-print items current-page)
           (message "Playlist view updated"))
         (message "No more playlists"))))))

(defun spotify-playlist-tracks ()
  "Displays the tracks that belongs to the playlist under the cursor."
  (interactive)
  (let* ((selected-playlist (tabulated-list-get-id))
         (name (spotify-get-item-name selected-playlist))
         (buffer (get-buffer-create (format "*Playlist Tracks: %s*" name))))
    (with-current-buffer buffer
      (spotify-track-search-mode)
      (setq-local spotify-selected-playlist selected-playlist)
      (spotify-playlist-tracks-update 1))))

(defun spotify-playlist-set-list-format ()
  "Configures the column data for the typical playlist view."
  (setq tabulated-list-format
        (vector `("Playlist Name" ,(- (window-width) 45) t)
                '("Owner Id" 30 t)
                '("# Tracks" 8 (lambda (row-1 row-2)
                                 (< (spotify-get-playlist-track-count (first row-1))
                                    (spotify-get-playlist-track-count (first row-2)))) :right-align t))))

(defun spotify-playlist-search-print (playlists current-page)
  "Appends the given playlists to the current playlist view."
  (let (entries)
    (dolist (playlist playlists)
      (let ((user-id (spotify-get-playlist-owner-id playlist))
            (playlist-name (spotify-get-item-name playlist)))
        (push (list playlist
                    (vector (cons playlist-name
                                  (list 'face 'link
                                        'follow-link t
                                        'action `(lambda (_) (spotify-playlist-tracks))
                                        'help-echo (format "Show %s's tracks" playlist-name)))
                            (cons user-id
                                  (list 'face 'link
                                        'follow-link t
                                        'action `(lambda (_) (spotify-user-playlists ,user-id))
                                        'help-echo (format "Show %s's public playlists" user-id)))
                            (number-to-string (spotify-get-playlist-track-count playlist))))
              entries)))
    (when (eq 1 current-page) (setq-local tabulated-list-entries nil))
    (spotify-playlist-set-list-format)
    (setq-local tabulated-list-entries (append tabulated-list-entries (nreverse entries)))
    (tabulated-list-init-header)
    (tabulated-list-print t)))


(provide 'spotify-playlist-search)
