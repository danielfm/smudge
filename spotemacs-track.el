;;; spotemacs-track.el --- Spotemacs track search major mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018 Daniel Fernandes Martins

;;; Commentary:

;; This library implements UI and a major mode for searching and acting on Spotify playlists.

;;; Code:

(require 'spotemacs-api)
(require 'spotemacs-controller)

(defvar spotemacs-current-page)
(defvar spotemacs-query)
(defvar spotemacs-selected-album)
(defvar spotemacs-recently-played)

(defvar spotemacs-track-search-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "M-RET") #'spotemacs-track-select)
    (define-key map (kbd "a")     #'spotemacs-track-add)
    (define-key map (kbd "l")     #'spotemacs-track-load-more)
    (define-key map (kbd "g")     #'spotemacs-track-reload)
    (define-key map (kbd "f")     #'spotemacs-track-playlist-follow)
    (define-key map (kbd "u")     #'spotemacs-track-playlist-unfollow)
    map)
  "Local keymap for `spotemacs-track-search-mode' buffers.")

(define-derived-mode spotemacs-track-search-mode tabulated-list-mode "Track-Search"
  "Major mode for displaying the track listing returned by a Spotify search.")

(defun spotemacs-track-select ()
  "Play the track, album or artist under the cursor.
If the cursor is on a button representing an artist or album, start playing that
 artist or album.  Otherwise, play the track selected."
  (interactive)
  (let ((button-type (spotemacs-track-selected-button-type)))
    (cond ((eq 'artist button-type)
	         (spotemacs-track-artist-select))
	        ((eq 'album button-type)
	         (spotemacs-track-album-select))
	        (t (spotemacs-track-select-default)))))

(defun spotemacs-track-select-default ()
  "Play the track under the cursor.
If the track list represents a playlist, the given track is played in the
context of that playlist; if the track list represents an album, the given
track is played in the context of that album.  Otherwise, it will be played
without a context."
  (interactive)
  (let* ((track (tabulated-list-get-id))
         (context (cond ((bound-and-true-p spotemacs-selected-playlist) spotemacs-selected-playlist)
                        ((bound-and-true-p spotemacs-selected-album) spotemacs-selected-album)
                        (t nil))))
    (spotemacs-controller-play-track track context)))

(defun spotemacs-track-selected-button-type ()
  "Get the type of button under the cursor."
  (let ((selected-button (button-at (point))))
    (when selected-button
      (button-get selected-button 'artist-or-album))))

(defun spotemacs-track-artist-select ()
  "Plays the artist of the track under the cursor."
  (interactive)
  (let* ((track (tabulated-list-get-id))
         (artist (spotemacs-api-get-track-artist track)))
    (spotemacs-controller-play-track track artist)))

(defun spotemacs-track-album-select ()
  "Plays the album of the track under the cursor."
  (interactive)
  (let* ((track (tabulated-list-get-id))
         (album (spotemacs-api-get-track-album track)))
    (spotemacs-controller-play-track track album)))

(defun spotemacs-track-playlist-follow ()
  "Add the current user as the follower of the selected playlist."
  (interactive)
  (if (bound-and-true-p spotemacs-selected-playlist)
      (let ((playlist spotemacs-selected-playlist))
        (when (y-or-n-p (format "Follow playlist '%s'? " (spotemacs-api-get-item-name playlist)))
          (spotemacs-api-playlist-follow
           playlist
           (lambda (_)
             (message (format "Followed playlist '%s'" (spotemacs-api-get-item-name playlist)))))))
    (message "Cannot Follow a playlist from here")))

(defun spotemacs-track-playlist-unfollow ()
  "Remove the current user as the follower of the selected playlist."
  (interactive)
  (if (bound-and-true-p spotemacs-selected-playlist)
      (let ((playlist spotemacs-selected-playlist))
        (when (y-or-n-p (format "Unfollow playlist '%s'? " (spotemacs-api-get-item-name playlist)))
          (spotemacs-api-playlist-unfollow
           playlist
           (lambda (_)
             (message (format "Unfollowed playlist '%s'" (spotemacs-api-get-item-name playlist)))))))
    (message "Cannot unfollow a playlist from here")))

(defun spotemacs-track-reload ()
  "Reloads the first page of results for the current track view."
  (interactive)
  (cond ((bound-and-true-p spotemacs-recently-played)
         (spotemacs-track-recently-played-tracks-update 1))
        ((bound-and-true-p spotemacs-selected-playlist)
         (spotemacs-track-playlist-tracks-update 1))
        ((bound-and-true-p spotemacs-query)
         (spotemacs-track-search-update spotemacs-query 1))
        ((bound-and-true-p spotemacs-selected-album)
         (spotemacs-track-album-tracks-update spotemacs-selected-album 1))))

(defun spotemacs-track-load-more ()
  "Load the next page of results for the current track view."
  (interactive)
  (cond ((bound-and-true-p spotemacs-recently-played)
         (spotemacs-track-recently-played-tracks-update (1+ spotemacs-current-page)))
        ((bound-and-true-p spotemacs-selected-playlist)
         (spotemacs-track-playlist-tracks-update (1+ spotemacs-current-page)))
        ((bound-and-true-p spotemacs-selected-album)
         (spotemacs-track-album-tracks-update spotemacs-selected-album (1+ spotemacs-current-page)))
        ((bound-and-true-p spotemacs-query)
         (spotemacs-track-search-update spotemacs-query (1+ spotemacs-current-page)))))

(defun spotemacs-track-search-update (query page)
  "Fetch the PAGE of results using QUERY at the search endpoint."
  (let ((buffer (current-buffer)))
    (spotemacs-api-search
     'track
     query
     page
     (lambda (json)
       (if-let ((items (spotemacs-api-get-search-track-items json)))
           (with-current-buffer buffer
             (setq-local spotemacs-current-page page)
             (setq-local spotemacs-query query)
             (pop-to-buffer buffer)
             (spotemacs-track-search-print items page)
             (message "Track view updated"))
         (message "No more tracks"))))))

(defun spotemacs-track-playlist-tracks-update (page)
  "Fetch PAGE of results for the current playlist."
  (when (bound-and-true-p spotemacs-selected-playlist)
    (let ((buffer (current-buffer)))
      (spotemacs-api-playlist-tracks
       spotemacs-selected-playlist
       page
       (lambda (json)
         (if-let ((items (spotemacs-api-get-playlist-tracks json)))
             (with-current-buffer buffer
               (setq-local spotemacs-current-page page)
               (pop-to-buffer buffer)
               (spotemacs-track-search-print items page)
               (message "Track view updated"))
           (message "No more tracks")))))))

(defun spotemacs-track-album-tracks-update (album page)
  "Fetch PAGE of of tracks for ALBUM."
  (let ((buffer (current-buffer)))
    (spotemacs-api-album-tracks
     album
     page
     (lambda (json)
       (if-let ((items (spotemacs-api-get-items json)))
           (with-current-buffer buffer
             (setq-local spotemacs-current-page page)
             (setq-local spotemacs-selected-album album)
             (pop-to-buffer buffer)
             (spotemacs-track-search-print items page)
             (message "Track view updated"))
         (message "No more tracks"))))))

(defun spotemacs-track-recently-played-tracks-update (page)
  "Fetch PAGE of results for the recently played tracks."
  (let ((buffer (current-buffer)))
    (spotemacs-api-recently-played
     page
     (lambda (json)
       (if-let ((items (spotemacs-api-get-playlist-tracks json)))
           (with-current-buffer buffer
             (setq-local spotemacs-current-page page)
             (setq-local spotemacs-recently-played t)
             (pop-to-buffer buffer)
             (spotemacs-track-search-print items page)
             (message "Track view updated"))
         (message "No more tracks"))))))

(defun spotemacs-track-search-set-list-format ()
  "Configure the column data for the typical track view.
Default to sortin tracks by number when listing the tracks from an album."
  (let* ((base-width (truncate (/ (- (window-width) 30) 3)))
         (default-width (if (bound-and-true-p spotemacs-selected-album) (+ base-width 4) base-width )))
    (when (not (bound-and-true-p spotemacs-selected-playlist))
      (setq tabulated-list-sort-key `("#" . nil)))
    (setq tabulated-list-format
          (vconcat (vector `("#" 3 ,(lambda (row-1 row-2)
                                      (< (+ (* 100 (spotemacs-api-get-disc-number (car row-1)))
                                            (spotemacs-api-get-track-number (car row-1)))
                                         (+ (* 100 (spotemacs-api-get-disc-number (car row-2)))
                                            (spotemacs-api-get-track-number (car row-2))))) :right-align t)
                           `("Track Name" ,default-width t)
                           `("Artist" ,default-width t)
                           `("Album" ,default-width t)
                           `("Time" 8 (lambda (row-1 row-2)
                                        (< (spotemacs-get-track-duration (car row-1))
                                           (spotemacs-get-track-duration (car row-2))))))
                   (when (not (bound-and-true-p spotemacs-selected-album))
                     (vector '("Popularity" 14 t)))))))

(defun spotemacs-track-search-print (songs page)
  "Append SONGS to the PAGE of track view."
  (let (entries)
    (dolist (song songs)
      (when (spotemacs-api-is-track-playable song)
        (let* ((artist-name (spotemacs-api-get-track-artist-name song))
               (album (or (spotemacs-api-get-track-album song) spotemacs-selected-album))
               (album-name (spotemacs-api-get-item-name album))
               (album (spotemacs-api-get-track-album song)))
          (push (list song
                      (vector (number-to-string (spotemacs-api-get-track-number song))
                              (spotemacs-api-get-item-name song)
                              (cons artist-name
                                    (list 'face 'link
                                          'follow-link t
                                          'action `(lambda (_) (spotemacs-track-search ,(format "artist:\"%s\"" artist-name)))
                                          'help-echo (format "Show %s's tracks" artist-name)
					                                'artist-or-album 'artist))
                              (cons album-name
                                    (list 'face 'link
                                          'follow-link t
                                          'action `(lambda (_) (spotemacs-album-tracks ,album))
                                          'help-echo (format "Show %s's tracks" album-name)
					                                'artist-or-album 'album))
                              (spotemacs-api-get-track-duration-formatted song)
                              (when (not (bound-and-true-p spotemacs-selected-album))
                                (spotemacs-api-popularity-bar (spotemacs-api-get-track-popularity song)))))
                entries))))
    (spotemacs-track-search-set-list-format)
    (when (eq 1 page) (setq-local tabulated-list-entries nil))
    (setq-local tabulated-list-entries (append tabulated-list-entries (nreverse entries)))
    (tabulated-list-init-header)
    (tabulated-list-print t)))

(defun spotemacs-track-album-tracks (album)
  "Open a new buffer that lists the tracks from ALBUM."
  (let ((buffer (get-buffer-create (format "*Album: %s*" (spotemacs-api-get-item-name album)))))
    (with-current-buffer buffer
      (spotemacs-track-search-mode)
      (spotemacs-track-album-tracks-update album 1))))

(defun spotemacs-track-select-playlist (callback)
  "Call CALLBACK with results of user playlist selection."
  (interactive)
  (spotemacs-api-current-user
   (lambda (user)
     (spotemacs-api-user-playlists
      (spotemacs-api-get-item-id user)
      1
      (lambda (json)
        (if-let* ((choices (mapcar (lambda (a)
                                     (list (spotemacs-api-get-item-name a) (spotemacs-api-get-item-id a)))
                                   (spotemacs-api-get-items json)))
                  (selected (completing-read "Select Playlist: " choices)))
            (when (not (string= "" selected))
              (funcall callback (cadr (assoc selected choices))))))))))

(defun spotemacs-track-add ()
  "Add the track under the cursor on a playlist.  Prompt for the playlist."
  (interactive)
  (let ((selected-track (tabulated-list-get-id)))
    (spotemacs-track-select-playlist
     (lambda (playlist)
       (spotemacs-api-current-user
        (lambda (user)
          (spotemacs-api-playlist-add-track
           (spotemacs-api-get-item-id user)
           playlist
           (spotemacs-api-get-item-uri selected-track)
           (lambda (_)
             (message "Song added.")))))))))

(provide 'spotemacs-track)
;;; spotemacs-track.el ends here
