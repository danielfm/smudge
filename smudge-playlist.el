;;; smudge-playlist.el --- Smudge playlist search major mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018 Daniel Fernandes Martins

;;; Commentary:

;; This library implements UI and a major mode for searching and acting on Spotify playlists.

;;; Code:

(require 'smudge-api)
(require 'smudge-controller)
(require 'smudge-track)

(defvar smudge-user-id)
(defvar smudge-current-page)
(defvar smudge-browse-message)
(defvar smudge-selected-playlist)
(defvar smudge-query)

(defvar smudge-playlist-search-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "M-RET") 'smudge-playlist-select)
    (define-key map (kbd "l")     'smudge-playlist-load-more)
    (define-key map (kbd "g")     'smudge-playlist-reload)
    (define-key map (kbd "f")     'smudge-playlist-follow)
    (define-key map (kbd "u")     'smudge-playlist-unfollow)
    map)
  "Local keymap for `smudge-playlist-search-mode' buffers.")

(define-derived-mode smudge-playlist-search-mode tabulated-list-mode "Playlist-Search"
  "Major mode for displaying the playlists returned by a Spotify search.")

(defun smudge-playlist-select ()
  "Plays the playlist under the cursor."
  (interactive)
  (let ((selected-playlist (tabulated-list-get-id)))
    (smudge-controller-play-track nil selected-playlist)))

(defun smudge-playlist-reload ()
  "Reloads the first page of results for the current playlist view."
  (interactive)
  (let ((page 1))
    (cond ((bound-and-true-p smudge-query)          (smudge-playlist-search-update smudge-query page))
          ((bound-and-true-p smudge-browse-message) (smudge-playlist-featured-playlists-update page))
          (t                                         (smudge-playlist-user-playlists-update smudge-user-id page)))))

(defun smudge-playlist-load-more ()
  "Load the next page of results for the current playlist view."
  (interactive)
  (let ((next-page (1+ smudge-current-page)))
    (cond ((bound-and-true-p smudge-query)          (smudge-playlist-search-update smudge-query next-page))
          ((bound-and-true-p smudge-browse-message) (smudge-playlist-featured-playlists-update next-page))
          (t                                         (smudge-playlist-user-playlists-update smudge-user-id next-page)))))

(defun smudge-playlist-follow ()
  "Add the current user as the follower of the playlist under the cursor."
  (interactive)
  (let* ((selected-playlist (tabulated-list-get-id))
                 (name (smudge-api-get-item-name selected-playlist)))
    (when (y-or-n-p (format "Follow playlist '%s'? " name))
      (smudge-api-playlist-follow
       selected-playlist
       (lambda (_)
         (message (format "Followed playlist '%s'" name)))))))

(defun smudge-playlist-unfollow ()
  "Remove the current user as the follower of the playlist under the cursor."
  (interactive)
  (let* ((selected-playlist (tabulated-list-get-id))
                 (name (smudge-api-get-item-name selected-playlist)))
    (when (y-or-n-p (format "Unfollow playlist '%s'? " name))
      (smudge-api-playlist-unfollow
       selected-playlist
       (lambda (_)
         (message (format "Unfollowed playlist '%s'" name)))))))

(defun smudge-playlist-search-update (query page)
  "Fetch the given PAGE of QUERY results using the search endpoint."
  (let ((buffer (current-buffer)))
    (smudge-api-search
     'playlist
     query
     page
     (lambda (playlists)
       (if-let ((items (smudge-api-get-search-playlist-items playlists)))
           (with-current-buffer buffer
             (setq-local smudge-current-page page)
             (setq-local smudge-query query)
             (pop-to-buffer buffer)
             (smudge-playlist-search-print items page)
             (message "Playlist view updated"))
         (message "No more playlists"))))))

(defun smudge-playlist-user-playlists-update (user-id page)
  "Fetch PAGE of results using the playlist endpoint for USER-ID."
  (let ((buffer (current-buffer)))
    (smudge-api-user-playlists
     user-id
     page
     (lambda (playlists)
       (if-let ((items (smudge-api-get-items playlists)))
         (with-current-buffer buffer
           (setq-local smudge-user-id user-id)
           (setq-local smudge-current-page page)
           (pop-to-buffer buffer)
           (smudge-playlist-search-print items page)
           (message "Playlist view updated"))
         (message "No more playlists"))))))

(defun smudge-playlist-featured-playlists-update (page)
  "Fetch PAGE of results using of Spotify's featured playlists."
  (let ((buffer (current-buffer)))
    (smudge-api-featured-playlists
     page
     (lambda (json)
       (if-let ((items (smudge-api-get-search-playlist-items json))
                (msg (smudge-api-get-message json)))
         (with-current-buffer buffer
           (setq-local smudge-current-page page)
           (setq-local smudge-browse-message msg)
           (pop-to-buffer buffer)
           (smudge-playlist-search-print items page)
           (message "Playlist view updated"))
         (message "No more playlists"))))))

(defun smudge-playlist-tracks ()
  "Displays the tracks that belongs to the playlist under the cursor."
  (interactive)
  (let* ((selected-playlist (tabulated-list-get-id))
         (name (smudge-api-get-item-name selected-playlist))
         (buffer (get-buffer-create (format "*Playlist Tracks: %s*" name))))
    (with-current-buffer buffer
      (smudge-track-search-mode)
      (setq-local smudge-selected-playlist selected-playlist)
      (smudge-track-playlist-tracks-update 1))))

(defun smudge-playlist-set-list-format ()
  "Configures the column data for the typical playlist view."
  (setq tabulated-list-format
        (vector `("Playlist Name" ,(- (window-width) 45) t)
                '("Owner Id" 30 t)
                '("# Tracks" 8 (lambda (row-1 row-2)
                                 (< (smudge-api-get-playlist-track-count (car row-1))
                                    (smudge-api-get-playlist-track-count (car row-2)))) :right-align t))))

(defun smudge-playlist-search-print (playlists page)
  "Append PLAYLISTS to PAGE of the current playlist view."
  (let (entries)
    (dolist (playlist playlists)
      (let ((user-id (smudge-api-get-playlist-owner-id playlist))
            (playlist-name (smudge-api-get-item-name playlist)))
        (push (list playlist
                    (vector (cons playlist-name
                                  (list 'face 'link
                                        'follow-link t
                                        'action `(lambda (_) (smudge-playlist-tracks))
                                        'help-echo (format "Show %s's tracks" playlist-name)))
                            (cons user-id
                                  (list 'face 'link
                                        'follow-link t
                                        'action `(lambda (_) (smudge-user-playlists ,user-id))
                                        'help-echo (format "Show %s's public playlists" user-id)))
                            (number-to-string (smudge-api-get-playlist-track-count playlist))))
              entries)))
    (when (eq 1 page) (setq-local tabulated-list-entries nil))
    (smudge-playlist-set-list-format)
    (setq-local tabulated-list-entries (append tabulated-list-entries (nreverse entries)))
    (tabulated-list-init-header)
    (tabulated-list-print t)))

(provide 'smudge-playlist)
;;; smudge-playlist.el ends here
