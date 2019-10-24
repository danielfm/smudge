;;; spotify-track-search.el --- Spotify.el track search major mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018 Daniel Fernandes Martins

;;; Commentary:

;; This library implements UI and a major mode for searching and acting on Spotify playlists.

;;; Code:

(require 'spotify-api)
(require 'spotify-controller)

(defvar spotify-current-page)
(defvar spotify-query)
(defvar spotify-selected-album)
(defvar spotify-recently-played)

(defvar spotify-track-search-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "M-RET") 'spotify-track-select)
    (define-key map (kbd "a")     'spotify-track-add)
    (define-key map (kbd "l")     'spotify-track-load-more)
    (define-key map (kbd "g")     'spotify-track-reload)
    (define-key map (kbd "f")     'spotify-track-playlist-follow)
    (define-key map (kbd "u")     'spotify-track-playlist-unfollow)
    map)
  "Local keymap for `spotify-track-search-mode' buffers.")

;; Enables the `spotify-remote-mode' in the track search buffer
(add-hook 'spotify-track-search-mode-hook 'spotify-remote-mode)

(define-derived-mode spotify-track-search-mode tabulated-list-mode "Track-Search"
  "Major mode for displaying the track listing returned by a Spotify search.")

(defun spotify-track-select ()
  "Play the track, album or artist under the cursor.
If the cursor is on a button representing an artist or album, start playing that
 artist or album.  Otherwise, play the track selected."
  (interactive)
  (let ((button-type (spotify-track-selected-button-type)))
    (cond ((eq 'artist button-type)
	         (spotify-track-artist-select))
	        ((eq 'album button-type)
	         (spotify-track-album-select))
	        (t (spotify-track-select-default)))))

(defun spotify-track-select-default ()
  "Play the track under the cursor.
If the track list represents a playlist, the given track is played in the
context of that playlist; if the track list represents an album, the given
track is played in the context of that album.  Otherwise, it will be played
without a context."
  (interactive)
  (let* ((track (tabulated-list-get-id))
         (context (cond ((bound-and-true-p spotify-selected-playlist) spotify-selected-playlist)
                        ((bound-and-true-p spotify-selected-album) spotify-selected-album)
                        (t nil))))
    (spotify-play-track track context)))

(defun spotify-track-selected-button-type ()
  "Get the type of button under the cursor."
  (let ((selected-button (button-at (point))))
    (when selected-button
      (button-get selected-button 'artist-or-album))))

(defun spotify-track-artist-select ()
  "Plays the artist of the track under the cursor."
  (interactive)
  (let* ((track (tabulated-list-get-id))
         (artist (spotify-get-track-artist track)))
    (spotify-play-track track artist)))

(defun spotify-track-album-select ()
  "Plays the album of the track under the cursor."
  (interactive)
  (let* ((track (tabulated-list-get-id))
         (album (spotify-get-track-album track)))
    (spotify-play-track track album)))

(defun spotify-track-playlist-follow ()
  "Add the current user as the follower of the selected playlist."
  (interactive)
  (if (bound-and-true-p spotify-selected-playlist)
      (let ((playlist spotify-selected-playlist))
        (when (y-or-n-p (format "Follow playlist '%s'? " (spotify-get-item-name playlist)))
          (spotify-api-playlist-follow
           playlist
           (lambda (_)
             (message (format "Followed playlist '%s'" (spotify-get-item-name playlist)))))))
    (message "Cannot Follow a playlist from here")))

(defun spotify-track-playlist-unfollow ()
  "Remove the current user as the follower of the selected playlist."
  (interactive)
  (if (bound-and-true-p spotify-selected-playlist)
      (let ((playlist spotify-selected-playlist))
        (when (y-or-n-p (format "Unfollow playlist '%s'? " (spotify-get-item-name playlist)))
          (spotify-api-playlist-unfollow
           playlist
           (lambda (_)
             (message (format "Unfollowed playlist '%s'" (spotify-get-item-name playlist)))))))
    (message "Cannot unfollow a playlist from here")))

(defun spotify-track-reload ()
  "Reloads the first page of results for the current track view."
  (interactive)
  (cond ((bound-and-true-p spotify-recently-played)
         (spotify-recently-played-tracks-update 1))
        ((bound-and-true-p spotify-selected-playlist)
         (spotify-playlist-tracks-update 1))
        ((bound-and-true-p spotify-query)
         (spotify-track-search-update spotify-query 1))
        ((bound-and-true-p spotify-selected-album)
         (spotify-album-tracks-update spotify-selected-album 1))))

(defun spotify-track-load-more ()
  "Load the next page of results for the current track view."
  (interactive)
  (cond ((bound-and-true-p spotify-recently-played)
         (spotify-recently-played-tracks-update (1+ spotify-current-page)))
        ((bound-and-true-p spotify-selected-playlist)
         (spotify-playlist-tracks-update (1+ spotify-current-page)))
        ((bound-and-true-p spotify-selected-album)
         (spotify-album-tracks-update spotify-selected-album (1+ spotify-current-page)))
        ((bound-and-true-p spotify-query)
         (spotify-track-search-update spotify-query (1+ spotify-current-page)))))

(defun spotify-track-search-update (query page)
  "Fetch the PAGE of results using QUERY at the search endpoint."
  (let ((buffer (current-buffer)))
    (spotify-api-search
     'track
     query
     page
     (lambda (json)
       (if-let ((items (spotify-get-search-track-items json)))
           (with-current-buffer buffer
             (setq-local spotify-current-page page)
             (setq-local spotify-query query)
             (pop-to-buffer buffer)
             (spotify-track-search-print items page)
             (message "Track view updated"))
         (message "No more tracks"))))))

(defun spotify-playlist-tracks-update (page)
  "Fetch PAGE of results for the current playlist."
  (when (bound-and-true-p spotify-selected-playlist)
    (let ((buffer (current-buffer)))
      (spotify-api-playlist-tracks
       spotify-selected-playlist
       page
       (lambda (json)
         (if-let ((items (spotify-get-playlist-tracks json)))
             (with-current-buffer buffer
               (setq-local spotify-current-page page)
               (pop-to-buffer buffer)
               (spotify-track-search-print items page)
               (message "Track view updated"))
           (message "No more tracks")))))))

(defun spotify-album-tracks-update (album page)
  "Fetch PAGE of of tracks for ALBUM."
  (let ((buffer (current-buffer)))
    (spotify-api-album-tracks
     album
     page
     (lambda (json)
       (if-let ((items (spotify-get-items json)))
           (with-current-buffer buffer
             (setq-local spotify-current-page page)
             (setq-local spotify-selected-album album)
             (pop-to-buffer buffer)
             (spotify-track-search-print items page)
             (message "Track view updated"))
         (message "No more tracks"))))))

(defun spotify-recently-played-tracks-update (page)
  "Fetch PAGE of results for the recently played tracks."
  (let ((buffer (current-buffer)))
    (spotify-api-recently-played
     page
     (lambda (json)
       (if-let ((items (spotify-get-playlist-tracks json)))
           (with-current-buffer buffer
             (setq-local spotify-current-page page)
             (setq-local spotify-recently-played t)
             (pop-to-buffer buffer)
             (spotify-track-search-print items page)
             (message "Track view updated"))
         (message "No more tracks"))))))

(defun spotify-track-search-set-list-format ()
  "Configure the column data for the typical track view.
Default to sortin tracks by number when listing the tracks from an album."
  (let* ((base-width (truncate (/ (- (window-width) 30) 3)))
         (default-width (if (bound-and-true-p spotify-selected-album) (+ base-width 4) base-width )))
    (when (not (bound-and-true-p spotify-selected-playlist))
      (setq tabulated-list-sort-key `("#" . nil)))
    (setq tabulated-list-format
          (vconcat (vector `("#" 3 ,(lambda (row-1 row-2)
                                      (< (+ (* 100 (spotify-get-disc-number (car row-1)))
                                            (spotify-get-track-number (car row-1)))
                                         (+ (* 100 (spotify-get-disc-number (car row-2)))
                                            (spotify-get-track-number (car row-2))))) :right-align t)
                           `("Track Name" ,default-width t)
                           `("Artist" ,default-width t)
                           `("Album" ,default-width t)
                           `("Time" 8 (lambda (row-1 row-2)
                                        (< (spotify-get-track-duration (car row-1))
                                           (spotify-get-track-duration (car row-2))))))
                   (when (not (bound-and-true-p spotify-selected-album))
                     (vector '("Popularity" 14 t)))))))

(defun spotify-track-search-print (songs page)
  "Append SONGS to the PAGE of track view."
  (let (entries)
    (dolist (song songs)
      (when (spotify-is-track-playable song)
        (let* ((artist-name (spotify-get-track-artist-name song))
               (album (or (spotify-get-track-album song) spotify-selected-album))
               (album-name (spotify-get-item-name album))
               (album (spotify-get-track-album song)))
          (push (list song
                      (vector (number-to-string (spotify-get-track-number song))
                              (spotify-get-item-name song)
                              (cons artist-name
                                    (list 'face 'link
                                          'follow-link t
                                          'action `(lambda (_) (spotify-track-search ,(format "artist:\"%s\"" artist-name)))
                                          'help-echo (format "Show %s's tracks" artist-name)
					                                'artist-or-album 'artist))
                              (cons album-name
                                    (list 'face 'link
                                          'follow-link t
                                          'action `(lambda (_) (spotify-album-tracks ,album))
                                          'help-echo (format "Show %s's tracks" album-name)
					                                'artist-or-album 'album))
                              (spotify-get-track-duration-formatted song)
                              (when (not (bound-and-true-p spotify-selected-album))
                                (spotify-popularity-bar (spotify-get-track-popularity song)))))
                entries))))
    (spotify-track-search-set-list-format)
    (when (eq 1 page) (setq-local tabulated-list-entries nil))
    (setq-local tabulated-list-entries (append tabulated-list-entries (nreverse entries)))
    (tabulated-list-init-header)
    (tabulated-list-print t)))

(defun spotify-album-tracks (album)
  "Open a new buffer that lists the tracks from ALBUM."
  (let ((buffer (get-buffer-create (format "*Album: %s*" (spotify-get-item-name album)))))
    (with-current-buffer buffer
      (spotify-track-search-mode)
      (spotify-album-tracks-update album 1))))

(defun spotify-select-playlist (callback)
  "Call CALLBACK with results of user playlist selection."
  (interactive)
  (spotify-current-user
   (lambda (user)
     (spotify-api-user-playlists
      (spotify-get-item-id user)
      1
      (lambda (json)
        (if-let* ((choices (mapcar (lambda (a)
                                     (list (spotify-get-item-name a) (spotify-get-item-id a)))
                                   (spotify-get-items json)))
                  (selected (completing-read "Select Playlist: " choices)))
            (when (not (string= "" selected))
              (funcall callback (cadr (assoc selected choices))))))))))

(defun spotify-track-add ()
  "Add the track under the cursor on a playlist.  Prompt for the playlist."
  (interactive)
  (let ((selected-track (tabulated-list-get-id)))
    (spotify-select-playlist
     (lambda (playlist)
       (spotify-current-user
        (lambda (user)
          (spotify-api-playlist-add-track
           (spotify-get-item-id user)
           playlist
           (spotify-get-item-uri selected-track)
           (lambda (_)
             (message "Song added.")))))))))

(provide 'spotify-track-search)
;;; spotify-track-search.el ends here
