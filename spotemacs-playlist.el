;;; spotemacs-playlist.el --- Spotemacs playlist search major mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018 Daniel Fernandes Martins

;;; Commentary:

;; This library implements UI and a major mode for searching and acting on Spotify playlists.

;;; Code:

(require 'spotemacs-api)
(require 'spotemacs-controller)
(require 'spotemacs-track)

(defvar spotemacs-user-id)
(defvar spotemacs-current-page)
(defvar spotemacs-browse-message)
(defvar spotemacs-selected-playlist)
(defvar spotemacs-query)

(defvar spotemacs-playlist-search-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "M-RET") 'spotemacs-playlist-select)
    (define-key map (kbd "l")     'spotemacs-playlist-load-more)
    (define-key map (kbd "g")     'spotemacs-playlist-reload)
    (define-key map (kbd "f")     'spotemacs-playlist-follow)
    (define-key map (kbd "u")     'spotemacs-playlist-unfollow)
    map)
  "Local keymap for `spotemacs-playlist-search-mode' buffers.")

(define-derived-mode spotemacs-playlist-search-mode tabulated-list-mode "Playlist-Search"
  "Major mode for displaying the playlists returned by a Spotify search.")

(defun spotemacs-playlist-select ()
  "Plays the playlist under the cursor."
  (interactive)
  (let ((selected-playlist (tabulated-list-get-id)))
    (spotemacs-controller-play-track nil selected-playlist)))

(defun spotemacs-playlist-reload ()
  "Reloads the first page of results for the current playlist view."
  (interactive)
  (let ((page 1))
    (cond ((bound-and-true-p spotemacs-query)          (spotemacs-playlist-search-update spotemacs-query page))
          ((bound-and-true-p spotemacs-browse-message) (spotemacs-playlist-featured-playlists-update page))
          (t                                         (spotemacs-playlist-user-playlists-update spotemacs-user-id page)))))

(defun spotemacs-playlist-load-more ()
  "Load the next page of results for the current playlist view."
  (interactive)
  (let ((next-page (1+ spotemacs-current-page)))
    (cond ((bound-and-true-p spotemacs-query)          (spotemacs-playlist-search-update spotemacs-query next-page))
          ((bound-and-true-p spotemacs-browse-message) (spotemacs-playlist-featured-playlists-update next-page))
          (t                                         (spotemacs-playlist-user-playlists-update spotemacs-user-id next-page)))))

(defun spotemacs-playlist-follow ()
  "Add the current user as the follower of the playlist under the cursor."
  (interactive)
  (let* ((selected-playlist (tabulated-list-get-id))
                 (name (spotemacs-api-get-item-name selected-playlist)))
    (when (y-or-n-p (format "Follow playlist '%s'? " name))
      (spotemacs-api-playlist-follow
       selected-playlist
       (lambda (_)
         (message (format "Followed playlist '%s'" name)))))))

(defun spotemacs-playlist-unfollow ()
  "Remove the current user as the follower of the playlist under the cursor."
  (interactive)
  (let* ((selected-playlist (tabulated-list-get-id))
                 (name (spotemacs-api-get-item-name selected-playlist)))
    (when (y-or-n-p (format "Unfollow playlist '%s'? " name))
      (spotemacs-api-playlist-unfollow
       selected-playlist
       (lambda (_)
         (message (format "Unfollowed playlist '%s'" name)))))))

(defun spotemacs-playlist-search-update (query page)
  "Fetch the given PAGE of QUERY results using the search endpoint."
  (let ((buffer (current-buffer)))
    (spotemacs-api-search
     'playlist
     query
     page
     (lambda (playlists)
       (if-let ((items (spotemacs-api-get-search-playlist-items playlists)))
           (with-current-buffer buffer
             (setq-local spotemacs-current-page page)
             (setq-local spotemacs-query query)
             (pop-to-buffer buffer)
             (spotemacs-playlist-search-print items page)
             (message "Playlist view updated"))
         (message "No more playlists"))))))

(defun spotemacs-playlist-user-playlists-update (user-id page)
  "Fetch PAGE of results using the playlist endpoint for USER-ID."
  (let ((buffer (current-buffer)))
    (spotemacs-api-user-playlists
     user-id
     page
     (lambda (playlists)
       (if-let ((items (spotemacs-api-get-items playlists)))
         (with-current-buffer buffer
           (setq-local spotemacs-user-id user-id)
           (setq-local spotemacs-current-page page)
           (pop-to-buffer buffer)
           (spotemacs-playlist-search-print items page)
           (message "Playlist view updated"))
         (message "No more playlists"))))))

(defun spotemacs-playlist-featured-playlists-update (page)
  "Fetch PAGE of results using of Spotify's featured playlists."
  (let ((buffer (current-buffer)))
    (spotemacs-api-featured-playlists
     page
     (lambda (json)
       (if-let ((items (spotemacs-api-get-search-playlist-items json))
                (msg (spotemacs-api-get-message json)))
         (with-current-buffer buffer
           (setq-local spotemacs-current-page page)
           (setq-local spotemacs-browse-message msg)
           (pop-to-buffer buffer)
           (spotemacs-playlist-search-print items page)
           (message "Playlist view updated"))
         (message "No more playlists"))))))

(defun spotemacs-playlist-tracks ()
  "Displays the tracks that belongs to the playlist under the cursor."
  (interactive)
  (let* ((selected-playlist (tabulated-list-get-id))
         (name (spotemacs-api-get-item-name selected-playlist))
         (buffer (get-buffer-create (format "*Playlist Tracks: %s*" name))))
    (with-current-buffer buffer
      (spotemacs-track-search-mode)
      (setq-local spotemacs-selected-playlist selected-playlist)
      (spotemacs-track-playlist-tracks-update 1))))

(defun spotemacs-playlist-set-list-format ()
  "Configures the column data for the typical playlist view."
  (setq tabulated-list-format
        (vector `("Playlist Name" ,(- (window-width) 45) t)
                '("Owner Id" 30 t)
                '("# Tracks" 8 (lambda (row-1 row-2)
                                 (< (spotemacs-api-get-playlist-track-count (car row-1))
                                    (spotemacs-api-get-playlist-track-count (car row-2)))) :right-align t))))

(defun spotemacs-playlist-search-print (playlists page)
  "Append PLAYLISTS to PAGE of the current playlist view."
  (let (entries)
    (dolist (playlist playlists)
      (let ((user-id (spotemacs-api-get-playlist-owner-id playlist))
            (playlist-name (spotemacs-api-get-item-name playlist)))
        (push (list playlist
                    (vector (cons playlist-name
                                  (list 'face 'link
                                        'follow-link t
                                        'action `(lambda (_) (spotemacs-playlist-tracks))
                                        'help-echo (format "Show %s's tracks" playlist-name)))
                            (cons user-id
                                  (list 'face 'link
                                        'follow-link t
                                        'action `(lambda (_) (spotemacs-user-playlists ,user-id))
                                        'help-echo (format "Show %s's public playlists" user-id)))
                            (number-to-string (spotemacs-api-get-playlist-track-count playlist))))
              entries)))
    (when (eq 1 page) (setq-local tabulated-list-entries nil))
    (spotemacs-playlist-set-list-format)
    (setq-local tabulated-list-entries (append tabulated-list-entries (nreverse entries)))
    (tabulated-list-init-header)
    (tabulated-list-print t)))

(provide 'spotemacs-playlist)
;;; spotemacs-playlist.el ends here
