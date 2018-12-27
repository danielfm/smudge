;; spotify-track-search.el --- Spotify.el track search major mode

;; Copyright (C) 2014-2018 Daniel Fernandes Martins

;; Code:

(require 'spotify-api)

(defvar spotify-track-search-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "M-RET") 'spotify-track-select)
    (define-key map (kbd "l")     'spotify-track-load-more)
    (define-key map (kbd "g")     'spotify-track-reload)
    (define-key map (kbd "f")     'spotify-track-playlist-follow)
    (define-key map (kbd "u")     'spotify-track-playlist-unfollow)
    map)
  "Local keymap for `spotify-track-search-mode' buffers.")

;; Enables the `spotify-remote-mode' the track search buffer
(add-hook 'spotify-track-search-mode-hook 'spotify-remote-mode)

(define-derived-mode spotify-track-search-mode tabulated-list-mode "Track-Search"
  "Major mode for displaying the track listing returned by a Spotify search.")

(defun spotify-track-select ()
  "Plays the track under the cursor. If the track list represents a playlist,
the given track is played in the context of that playlist; otherwise, it will
be played in the context of its album."
  (interactive)
  (let ((selected-track (tabulated-list-get-id)))
    (if (bound-and-true-p spotify-selected-playlist)
        (spotify-play-track selected-track spotify-selected-playlist)
      (spotify-play-track selected-track (spotify-get-track-album selected-track)))))

(defun spotify-track-playlist-follow ()
  "Adds the current user as the follower of the selected playlist."
  (interactive)
  (if (bound-and-true-p spotify-selected-playlist)
      (when (and (y-or-n-p (format "Follow playlist '%s'?" (spotify-get-item-name spotify-selected-playlist)))
                 (spotify-api-playlist-follow spotify-selected-playlist))
        (message (format "Followed playlist '%s'" (spotify-get-item-name spotify-selected-playlist))))
    (message "Cannot follow a playlist from here")))

(defun spotify-track-playlist-unfollow ()
  "Removes the current user as the follower of the selected playlist."
  (interactive)
  (if (bound-and-true-p spotify-selected-playlist)
      (when (and (y-or-n-p (format "Unfollow playlist '%s'?" (spotify-get-item-name spotify-selected-playlist)))
                 (spotify-api-playlist-unfollow spotify-selected-playlist))
        (message (format "Unfollowed playlist '%s'" (spotify-get-item-name spotify-selected-playlist))))
    (message "Cannot unfollow a playlist from here")))

(defun spotify-track-reload ()
  "Reloads the first page of results for the current track view."
  (interactive)
  (if (bound-and-true-p spotify-query)
      (spotify-track-search-update 1)
    (spotify-playlist-tracks-update 1)))

(defun spotify-track-load-more ()
  "Loads the next page of results for the current track view."
  (interactive)
  (if (bound-and-true-p spotify-query)
      (spotify-track-search-update (1+ spotify-current-page))
    (spotify-playlist-tracks-update (1+ spotify-current-page))))

(defun spotify-track-search-update (current-page)
  "Fetches the given page of results using the search endpoint."
  (let* ((json (spotify-api-search 'track spotify-query current-page))
         (items (spotify-get-search-track-items json)))
    (if items
        (progn
          (spotify-track-search-print items current-page)
          (message "Track view updated"))
      (message "No more tracks"))))

(defun spotify-playlist-tracks-update (current-page)
  "Fetches the given page of results for the current playlist."
  (when (bound-and-true-p spotify-selected-playlist)
    (let* ((json (spotify-api-playlist-tracks spotify-selected-playlist current-page))
           (items (spotify-get-playlist-tracks json)))
      (if items
          (progn
            (spotify-track-search-print items current-page)
            (message "Track view updated"))
        (message "No more tracks")))))

(defun spotify-track-search-set-list-format ()
  "Configures the column data for the typical track view."
  (let ((default-width (truncate (/ (- (window-width) 30) 3))))
    (setq tabulated-list-format
          (vector '("#" 3 nil :right-align t)
                  `("Track Name" ,default-width t)
                  `("Artist" ,default-width t)
                  `("Album" ,default-width t)
                  `("Time" 8 (lambda (row-1 row-2)
                                (< (spotify-get-track-duration (first row-1))
                                   (spotify-get-track-duration (first row-2)))))
                  '("Popularity" 14 t)))))

(defun spotify-track-search-print (songs current-page)
  "Appends the given songs to the current track view."
  (let (entries)
    (dolist (song songs)
      (when (spotify-is-track-playable song)
        (let ((artist-name (spotify-get-track-artist song))
              (album-name (spotify-get-track-album-name song)))
          (push (list song
                      (vector (number-to-string (spotify-get-track-number song))
                              (spotify-get-item-name song)
                              (cons artist-name
                                  (list 'face 'link
                                        'follow-link t
                                        'action `(lambda (_) (spotify-track-search ,(format "artist:\"%s\"" artist-name)))
                                        'help-echo (format "Show %s's tracks" artist-name)))
                              (cons album-name
                                  (list 'face 'link
                                        'follow-link t
                                        'action `(lambda (_) (spotify-track-search ,(format "artist:\"%s\" album:\"%s\"" artist-name album-name)))
                                        'help-echo (format "Show %s's tracks" album-name)))
                              (spotify-get-track-duration-formatted song)
                              (spotify-popularity-bar (spotify-get-track-popularity song))))
                entries))))
    (when (eq 1 current-page)
      (setq-local tabulated-list-entries nil))
    (setq-local tabulated-list-entries (append tabulated-list-entries (nreverse entries)))
    (setq-local spotify-current-page current-page)
    (spotify-track-search-set-list-format)
    (tabulated-list-init-header)
    (tabulated-list-print t)))

(provide 'spotify-track-search)
