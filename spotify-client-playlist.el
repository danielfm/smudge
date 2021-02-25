;;; spotify-client-playlist.el --- spotify-client playlist search major mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018 Daniel Fernandes Martins

;;; Commentary:

;; This library implements UI and a major mode for searching and acting on Spotify playlists.

;;; Code:

(require 'spotify-client-api)
(require 'spotify-client-controller)
(require 'spotify-client-track)

(defvar spotify-client-user-id)
(defvar spotify-client-current-page)
(defvar spotify-client-browse-message)
(defvar spotify-client-selected-playlist)
(defvar spotify-client-query)

(defvar spotify-client-playlist-search-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "M-RET") 'spotify-client-playlist-select)
    (define-key map (kbd "l")     'spotify-client-playlist-load-more)
    (define-key map (kbd "g")     'spotify-client-playlist-reload)
    (define-key map (kbd "f")     'spotify-client-playlist-follow)
    (define-key map (kbd "u")     'spotify-client-playlist-unfollow)
    map)
  "Local keymap for `spotify-client-playlist-search-mode' buffers.")

(define-derived-mode spotify-client-playlist-search-mode tabulated-list-mode "Playlist-Search"
  "Major mode for displaying the playlists returned by a Spotify search.")

(defun spotify-client-playlist-select ()
  "Plays the playlist under the cursor."
  (interactive)
  (let ((selected-playlist (tabulated-list-get-id)))
    (spotify-client-controller-play-track nil selected-playlist)))

(defun spotify-client-playlist-reload ()
  "Reloads the first page of results for the current playlist view."
  (interactive)
  (let ((page 1))
    (cond ((bound-and-true-p spotify-client-query)          (spotify-client-playlist-search-update spotify-client-query page))
          ((bound-and-true-p spotify-client-browse-message) (spotify-client-playlist-featured-playlists-update page))
          (t                                         (spotify-client-playlist-user-playlists-update spotify-client-user-id page)))))

(defun spotify-client-playlist-load-more ()
  "Load the next page of results for the current playlist view."
  (interactive)
  (let ((next-page (1+ spotify-client-current-page)))
    (cond ((bound-and-true-p spotify-client-query)          (spotify-client-playlist-search-update spotify-client-query next-page))
          ((bound-and-true-p spotify-client-browse-message) (spotify-client-playlist-featured-playlists-update next-page))
          (t                                         (spotify-client-playlist-user-playlists-update spotify-client-user-id next-page)))))

(defun spotify-client-playlist-follow ()
  "Add the current user as the follower of the playlist under the cursor."
  (interactive)
  (let* ((selected-playlist (tabulated-list-get-id))
                 (name (spotify-client-api-get-item-name selected-playlist)))
    (when (y-or-n-p (format "Follow playlist '%s'? " name))
      (spotify-client-api-playlist-follow
       selected-playlist
       (lambda (_)
         (message (format "Followed playlist '%s'" name)))))))

(defun spotify-client-playlist-unfollow ()
  "Remove the current user as the follower of the playlist under the cursor."
  (interactive)
  (let* ((selected-playlist (tabulated-list-get-id))
                 (name (spotify-client-api-get-item-name selected-playlist)))
    (when (y-or-n-p (format "Unfollow playlist '%s'? " name))
      (spotify-client-api-playlist-unfollow
       selected-playlist
       (lambda (_)
         (message (format "Unfollowed playlist '%s'" name)))))))

(defun spotify-client-playlist-search-update (query page)
  "Fetch the given PAGE of QUERY results using the search endpoint."
  (let ((buffer (current-buffer)))
    (spotify-client-api-search
     'playlist
     query
     page
     (lambda (playlists)
       (if-let ((items (spotify-client-api-get-search-playlist-items playlists)))
           (with-current-buffer buffer
             (setq-local spotify-client-current-page page)
             (setq-local spotify-client-query query)
             (pop-to-buffer buffer)
             (spotify-client-playlist-search-print items page)
             (message "Playlist view updated"))
         (message "No more playlists"))))))

(defun spotify-client-playlist-user-playlists-update (user-id page)
  "Fetch PAGE of results using the playlist endpoint for USER-ID."
  (let ((buffer (current-buffer)))
    (spotify-client-api-user-playlists
     user-id
     page
     (lambda (playlists)
       (if-let ((items (spotify-client-api-get-items playlists)))
         (with-current-buffer buffer
           (setq-local spotify-client-user-id user-id)
           (setq-local spotify-client-current-page page)
           (pop-to-buffer buffer)
           (spotify-client-playlist-search-print items page)
           (message "Playlist view updated"))
         (message "No more playlists"))))))

(defun spotify-client-playlist-featured-playlists-update (page)
  "Fetch PAGE of results using of Spotify's featured playlists."
  (let ((buffer (current-buffer)))
    (spotify-client-api-featured-playlists
     page
     (lambda (json)
       (if-let ((items (spotify-client-api-get-search-playlist-items json))
                (msg (spotify-client-api-get-message json)))
         (with-current-buffer buffer
           (setq-local spotify-client-current-page page)
           (setq-local spotify-client-browse-message msg)
           (pop-to-buffer buffer)
           (spotify-client-playlist-search-print items page)
           (message "Playlist view updated"))
         (message "No more playlists"))))))

(defun spotify-client-playlist-tracks ()
  "Displays the tracks that belongs to the playlist under the cursor."
  (interactive)
  (let* ((selected-playlist (tabulated-list-get-id))
         (name (spotify-client-api-get-item-name selected-playlist))
         (buffer (get-buffer-create (format "*Playlist Tracks: %s*" name))))
    (with-current-buffer buffer
      (spotify-client-track-search-mode)
      (setq-local spotify-client-selected-playlist selected-playlist)
      (spotify-client-track-playlist-tracks-update 1))))

(defun spotify-client-playlist-set-list-format ()
  "Configures the column data for the typical playlist view."
  (setq tabulated-list-format
        (vector `("Playlist Name" ,(- (window-width) 45) t)
                '("Owner Id" 30 t)
                '("# Tracks" 8 (lambda (row-1 row-2)
                                 (< (spotify-client-api-get-playlist-track-count (car row-1))
                                    (spotify-client-api-get-playlist-track-count (car row-2)))) :right-align t))))

(defun spotify-client-playlist-search-print (playlists page)
  "Append PLAYLISTS to PAGE of the current playlist view."
  (let (entries)
    (dolist (playlist playlists)
      (let ((user-id (spotify-client-api-get-playlist-owner-id playlist))
            (playlist-name (spotify-client-api-get-item-name playlist)))
        (push (list playlist
                    (vector (cons playlist-name
                                  (list 'face 'link
                                        'follow-link t
                                        'action `(lambda (_) (spotify-client-playlist-tracks))
                                        'help-echo (format "Show %s's tracks" playlist-name)))
                            (cons user-id
                                  (list 'face 'link
                                        'follow-link t
                                        'action `(lambda (_) (spotify-client-user-playlists ,user-id))
                                        'help-echo (format "Show %s's public playlists" user-id)))
                            (number-to-string (spotify-client-api-get-playlist-track-count playlist))))
              entries)))
    (when (eq 1 page) (setq-local tabulated-list-entries nil))
    (spotify-client-playlist-set-list-format)
    (setq-local tabulated-list-entries (append tabulated-list-entries (nreverse entries)))
    (tabulated-list-init-header)
    (tabulated-list-print t)))

(provide 'spotify-client-playlist)
;;; spotify-client-playlist.el ends here
