;;; spotify-client-track.el --- spotify-client track search major mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018 Daniel Fernandes Martins

;;; Commentary:

;; This library implements UI and a major mode for searching and acting on Spotify playlists.

;;; Code:

(require 'spotify-client-api)
(require 'spotify-client-controller)

(defvar spotify-client-current-page)
(defvar spotify-client-query)
(defvar spotify-client-selected-album)
(defvar spotify-client-recently-played)

(defvar spotify-client-track-search-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "M-RET") #'spotify-client-track-select)
    (define-key map (kbd "a")     #'spotify-client-track-add)
    (define-key map (kbd "l")     #'spotify-client-track-load-more)
    (define-key map (kbd "g")     #'spotify-client-track-reload)
    (define-key map (kbd "f")     #'spotify-client-track-playlist-follow)
    (define-key map (kbd "u")     #'spotify-client-track-playlist-unfollow)
    map)
  "Local keymap for `spotify-client-track-search-mode' buffers.")

(define-derived-mode spotify-client-track-search-mode tabulated-list-mode "Track-Search"
  "Major mode for displaying the track listing returned by a Spotify search.")

(defun spotify-client-track-select ()
  "Play the track, album or artist under the cursor.
If the cursor is on a button representing an artist or album, start playing that
 artist or album.  Otherwise, play the track selected."
  (interactive)
  (let ((button-type (spotify-client-track-selected-button-type)))
    (cond ((eq 'artist button-type)
	         (spotify-client-track-artist-select))
	        ((eq 'album button-type)
	         (spotify-client-track-album-select))
	        (t (spotify-client-track-select-default)))))

(defun spotify-client-track-select-default ()
  "Play the track under the cursor.
If the track list represents a playlist, the given track is played in the
context of that playlist; if the track list represents an album, the given
track is played in the context of that album.  Otherwise, it will be played
without a context."
  (interactive)
  (let* ((track (tabulated-list-get-id))
         (context (cond ((bound-and-true-p spotify-client-selected-playlist) spotify-client-selected-playlist)
                        ((bound-and-true-p spotify-client-selected-album) spotify-client-selected-album)
                        (t nil))))
    (spotify-client-controller-play-track track context)))

(defun spotify-client-track-selected-button-type ()
  "Get the type of button under the cursor."
  (let ((selected-button (button-at (point))))
    (when selected-button
      (button-get selected-button 'artist-or-album))))

(defun spotify-client-track-artist-select ()
  "Plays the artist of the track under the cursor."
  (interactive)
  (let* ((track (tabulated-list-get-id))
         (artist (spotify-client-api-get-track-artist track)))
    (spotify-client-controller-play-track track artist)))

(defun spotify-client-track-album-select ()
  "Plays the album of the track under the cursor."
  (interactive)
  (let* ((track (tabulated-list-get-id))
         (album (spotify-client-api-get-track-album track)))
    (spotify-client-controller-play-track track album)))

(defun spotify-client-track-playlist-follow ()
  "Add the current user as the follower of the selected playlist."
  (interactive)
  (if (bound-and-true-p spotify-client-selected-playlist)
      (let ((playlist spotify-client-selected-playlist))
        (when (y-or-n-p (format "Follow playlist '%s'? " (spotify-client-api-get-item-name playlist)))
          (spotify-client-api-playlist-follow
           playlist
           (lambda (_)
             (message (format "Followed playlist '%s'" (spotify-client-api-get-item-name playlist)))))))
    (message "Cannot Follow a playlist from here")))

(defun spotify-client-track-playlist-unfollow ()
  "Remove the current user as the follower of the selected playlist."
  (interactive)
  (if (bound-and-true-p spotify-client-selected-playlist)
      (let ((playlist spotify-client-selected-playlist))
        (when (y-or-n-p (format "Unfollow playlist '%s'? " (spotify-client-api-get-item-name playlist)))
          (spotify-client-api-playlist-unfollow
           playlist
           (lambda (_)
             (message (format "Unfollowed playlist '%s'" (spotify-client-api-get-item-name playlist)))))))
    (message "Cannot unfollow a playlist from here")))

(defun spotify-client-track-reload ()
  "Reloads the first page of results for the current track view."
  (interactive)
  (cond ((bound-and-true-p spotify-client-recently-played)
         (spotify-client-track-recently-played-tracks-update 1))
        ((bound-and-true-p spotify-client-selected-playlist)
         (spotify-client-track-playlist-tracks-update 1))
        ((bound-and-true-p spotify-client-query)
         (spotify-client-track-search-update spotify-client-query 1))
        ((bound-and-true-p spotify-client-selected-album)
         (spotify-client-track-album-tracks-update spotify-client-selected-album 1))))

(defun spotify-client-track-load-more ()
  "Load the next page of results for the current track view."
  (interactive)
  (cond ((bound-and-true-p spotify-client-recently-played)
         (spotify-client-track-recently-played-tracks-update (1+ spotify-client-current-page)))
        ((bound-and-true-p spotify-client-selected-playlist)
         (spotify-client-track-playlist-tracks-update (1+ spotify-client-current-page)))
        ((bound-and-true-p spotify-client-selected-album)
         (spotify-client-track-album-tracks-update spotify-client-selected-album (1+ spotify-client-current-page)))
        ((bound-and-true-p spotify-client-query)
         (spotify-client-track-search-update spotify-client-query (1+ spotify-client-current-page)))))

(defun spotify-client-track-search-update (query page)
  "Fetch the PAGE of results using QUERY at the search endpoint."
  (let ((buffer (current-buffer)))
    (spotify-client-api-search
     'track
     query
     page
     (lambda (json)
       (if-let ((items (spotify-client-api-get-search-track-items json)))
           (with-current-buffer buffer
             (setq-local spotify-client-current-page page)
             (setq-local spotify-client-query query)
             (pop-to-buffer buffer)
             (spotify-client-track-search-print items page)
             (message "Track view updated"))
         (message "No more tracks"))))))

(defun spotify-client-track-playlist-tracks-update (page)
  "Fetch PAGE of results for the current playlist."
  (when (bound-and-true-p spotify-client-selected-playlist)
    (let ((buffer (current-buffer)))
      (spotify-client-api-playlist-tracks
       spotify-client-selected-playlist
       page
       (lambda (json)
         (if-let ((items (spotify-client-api-get-playlist-tracks json)))
             (with-current-buffer buffer
               (setq-local spotify-client-current-page page)
               (pop-to-buffer buffer)
               (spotify-client-track-search-print items page)
               (message "Track view updated"))
           (message "No more tracks")))))))

(defun spotify-client-track-album-tracks-update (album page)
  "Fetch PAGE of of tracks for ALBUM."
  (let ((buffer (current-buffer)))
    (spotify-client-api-album-tracks
     album
     page
     (lambda (json)
       (if-let ((items (spotify-client-api-get-items json)))
           (with-current-buffer buffer
             (setq-local spotify-client-current-page page)
             (setq-local spotify-client-selected-album album)
             (pop-to-buffer buffer)
             (spotify-client-track-search-print items page)
             (message "Track view updated"))
         (message "No more tracks"))))))

(defun spotify-client-track-recently-played-tracks-update (page)
  "Fetch PAGE of results for the recently played tracks."
  (let ((buffer (current-buffer)))
    (spotify-client-api-recently-played
     page
     (lambda (json)
       (if-let ((items (spotify-client-api-get-playlist-tracks json)))
           (with-current-buffer buffer
             (setq-local spotify-client-current-page page)
             (setq-local spotify-client-recently-played t)
             (pop-to-buffer buffer)
             (spotify-client-track-search-print items page)
             (message "Track view updated"))
         (message "No more tracks"))))))

(defun spotify-client-track-search-set-list-format ()
  "Configure the column data for the typical track view.
Default to sortin tracks by number when listing the tracks from an album."
  (let* ((base-width (truncate (/ (- (window-width) 30) 3)))
         (default-width (if (bound-and-true-p spotify-client-selected-album) (+ base-width 4) base-width )))
    (when (not (bound-and-true-p spotify-client-selected-playlist))
      (setq tabulated-list-sort-key `("#" . nil)))
    (setq tabulated-list-format
          (vconcat (vector `("#" 3 ,(lambda (row-1 row-2)
                                      (< (+ (* 100 (spotify-client-api-get-disc-number (car row-1)))
                                            (spotify-client-api-get-track-number (car row-1)))
                                         (+ (* 100 (spotify-client-api-get-disc-number (car row-2)))
                                            (spotify-client-api-get-track-number (car row-2))))) :right-align t)
                           `("Track Name" ,default-width t)
                           `("Artist" ,default-width t)
                           `("Album" ,default-width t)
                           `("Time" 8 (lambda (row-1 row-2)
                                        (< (spotify-client-get-track-duration (car row-1))
                                           (spotify-client-get-track-duration (car row-2))))))
                   (when (not (bound-and-true-p spotify-client-selected-album))
                     (vector '("Popularity" 14 t)))))))

(defun spotify-client-track-search-print (songs page)
  "Append SONGS to the PAGE of track view."
  (let (entries)
    (dolist (song songs)
      (when (spotify-client-api-is-track-playable song)
        (let* ((artist-name (spotify-client-api-get-track-artist-name song))
               (album (or (spotify-client-api-get-track-album song) spotify-client-selected-album))
               (album-name (spotify-client-api-get-item-name album))
               (album (spotify-client-api-get-track-album song)))
          (push (list song
                      (vector (number-to-string (spotify-client-api-get-track-number song))
                              (spotify-client-api-get-item-name song)
                              (cons artist-name
                                    (list 'face 'link
                                          'follow-link t
                                          'action `(lambda (_) (spotify-client-track-search ,(format "artist:\"%s\"" artist-name)))
                                          'help-echo (format "Show %s's tracks" artist-name)
					                                'artist-or-album 'artist))
                              (cons album-name
                                    (list 'face 'link
                                          'follow-link t
                                          'action `(lambda (_) (spotify-client-track-album-tracks ,album))
                                          'help-echo (format "Show %s's tracks" album-name)
					                                'artist-or-album 'album))
                              (spotify-client-api-get-track-duration-formatted song)
                              (when (not (bound-and-true-p spotify-client-selected-album))
                                (spotify-client-api-popularity-bar (spotify-client-api-get-track-popularity song)))))
                entries))))
    (spotify-client-track-search-set-list-format)
    (when (eq 1 page) (setq-local tabulated-list-entries nil))
    (setq-local tabulated-list-entries (append tabulated-list-entries (nreverse entries)))
    (tabulated-list-init-header)
    (tabulated-list-print t)))

(defun spotify-client-track-album-tracks (album)
  "Open a new buffer that lists the tracks from ALBUM."
  (let ((buffer (get-buffer-create (format "*Album: %s*" (spotify-client-api-get-item-name album)))))
    (with-current-buffer buffer
      (spotify-client-track-search-mode)
      (spotify-client-track-album-tracks-update album 1))))

(defun spotify-client-track-select-playlist (callback)
  "Call CALLBACK with results of user playlist selection."
  (interactive)
  (spotify-client-api-current-user
   (lambda (user)
     (spotify-client-api-user-playlists
      (spotify-client-api-get-item-id user)
      1
      (lambda (json)
        (if-let* ((choices (mapcar (lambda (a)
                                     (list (spotify-client-api-get-item-name a) (spotify-client-api-get-item-id a)))
                                   (spotify-client-api-get-items json)))
                  (selected (completing-read "Select Playlist: " choices)))
            (when (not (string= "" selected))
              (funcall callback (cadr (assoc selected choices))))))))))

(defun spotify-client-track-add ()
  "Add the track under the cursor on a playlist.  Prompt for the playlist."
  (interactive)
  (let ((selected-track (tabulated-list-get-id)))
    (spotify-client-track-select-playlist
     (lambda (playlist)
       (spotify-client-api-current-user
        (lambda (user)
          (spotify-client-api-playlist-add-track
           (spotify-client-api-get-item-id user)
           playlist
           (spotify-client-api-get-item-uri selected-track)
           (lambda (_)
             (message "Song added.")))))))))

(provide 'spotify-client-track)
;;; spotify-client-track.el ends here
