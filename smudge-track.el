;;; smudge-track.el --- Smudge track search major mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018 Daniel Fernandes Martins

;;; Commentary:

;; This library implements UI and a major mode for searching and acting on Spotify playlists.

;;; Code:

(require 'smudge-api)
(require 'smudge-controller)

(defvar smudge-current-page)
(defvar smudge-query)
(defvar smudge-selected-album)
(defvar smudge-recently-played)

(defvar smudge-track-search-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "M-RET") #'smudge-track-select)
    (define-key map (kbd "a")     #'smudge-track-add)
    (define-key map (kbd "l")     #'smudge-track-load-more)
    (define-key map (kbd "g")     #'smudge-track-reload)
    (define-key map (kbd "f")     #'smudge-track-playlist-follow)
    (define-key map (kbd "u")     #'smudge-track-playlist-unfollow)
    map)
  "Local keymap for `smudge-track-search-mode' buffers.")

(define-derived-mode smudge-track-search-mode tabulated-list-mode "Track-Search"
  "Major mode for displaying the track listing returned by a Spotify search.")

(defun smudge-track-select ()
  "Play the track, album or artist under the cursor.
If the cursor is on a button representing an artist or album, start playing that
 artist or album.  Otherwise, play the track selected."
  (interactive)
  (let ((button-type (smudge-track-selected-button-type)))
    (cond ((eq 'artist button-type)
	         (smudge-track-artist-select))
	        ((eq 'album button-type)
	         (smudge-track-album-select))
	        (t (smudge-track-select-default)))))

(defun smudge-track-select-default ()
  "Play the track under the cursor.
If the track list represents a playlist, the given track is played in the
context of that playlist; if the track list represents an album, the given
track is played in the context of that album.  Otherwise, it will be played
without a context."
  (interactive)
  (let* ((track (tabulated-list-get-id))
         (context (cond ((bound-and-true-p smudge-selected-playlist) smudge-selected-playlist)
                        ((bound-and-true-p smudge-selected-album) smudge-selected-album)
                        (t nil))))
    (smudge-controller-play-track track context)))

(defun smudge-track-selected-button-type ()
  "Get the type of button under the cursor."
  (let ((selected-button (button-at (point))))
    (when selected-button
      (button-get selected-button 'artist-or-album))))

(defun smudge-track-artist-select ()
  "Plays the artist of the track under the cursor."
  (interactive)
  (let* ((track (tabulated-list-get-id))
         (artist (smudge-api-get-track-artist track)))
    (smudge-controller-play-track track artist)))

(defun smudge-track-album-select ()
  "Plays the album of the track under the cursor."
  (interactive)
  (let* ((track (tabulated-list-get-id))
         (album (smudge-api-get-track-album track)))
    (smudge-controller-play-track track album)))

(defun smudge-track-playlist-follow ()
  "Add the current user as the follower of the selected playlist."
  (interactive)
  (if (bound-and-true-p smudge-selected-playlist)
      (let ((playlist smudge-selected-playlist))
        (when (y-or-n-p (format "Follow playlist '%s'? " (smudge-api-get-item-name playlist)))
          (smudge-api-playlist-follow
           playlist
           (lambda (_)
             (message (format "Followed playlist '%s'" (smudge-api-get-item-name playlist)))))))
    (message "Cannot Follow a playlist from here")))

(defun smudge-track-playlist-unfollow ()
  "Remove the current user as the follower of the selected playlist."
  (interactive)
  (if (bound-and-true-p smudge-selected-playlist)
      (let ((playlist smudge-selected-playlist))
        (when (y-or-n-p (format "Unfollow playlist '%s'? " (smudge-api-get-item-name playlist)))
          (smudge-api-playlist-unfollow
           playlist
           (lambda (_)
             (message (format "Unfollowed playlist '%s'" (smudge-api-get-item-name playlist)))))))
    (message "Cannot unfollow a playlist from here")))

(defun smudge-track-reload ()
  "Reloads the first page of results for the current track view."
  (interactive)
  (cond ((bound-and-true-p smudge-recently-played)
         (smudge-track-recently-played-tracks-update 1))
        ((bound-and-true-p smudge-selected-playlist)
         (smudge-track-playlist-tracks-update 1))
        ((bound-and-true-p smudge-query)
         (smudge-track-search-update smudge-query 1))
        ((bound-and-true-p smudge-selected-album)
         (smudge-track-album-tracks-update smudge-selected-album 1))))

(defun smudge-track-load-more ()
  "Load the next page of results for the current track view."
  (interactive)
  (cond ((bound-and-true-p smudge-recently-played)
         (smudge-track-recently-played-tracks-update (1+ smudge-current-page)))
        ((bound-and-true-p smudge-selected-playlist)
         (smudge-track-playlist-tracks-update (1+ smudge-current-page)))
        ((bound-and-true-p smudge-selected-album)
         (smudge-track-album-tracks-update smudge-selected-album (1+ smudge-current-page)))
        ((bound-and-true-p smudge-query)
         (smudge-track-search-update smudge-query (1+ smudge-current-page)))))

(defun smudge-track-search-update (query page)
  "Fetch the PAGE of results using QUERY at the search endpoint."
  (let ((buffer (current-buffer)))
    (smudge-api-search
     'track
     query
     page
     (lambda (json)
       (if-let ((items (smudge-api-get-search-track-items json)))
           (with-current-buffer buffer
             (setq-local smudge-current-page page)
             (setq-local smudge-query query)
             (pop-to-buffer buffer)
             (smudge-track-search-print items page)
             (message "Track view updated"))
         (message "No more tracks"))))))

(defun smudge-track-playlist-tracks-update (page)
  "Fetch PAGE of results for the current playlist."
  (when (bound-and-true-p smudge-selected-playlist)
    (let ((buffer (current-buffer)))
      (smudge-api-playlist-tracks
       smudge-selected-playlist
       page
       (lambda (json)
         (if-let ((items (smudge-api-get-playlist-tracks json)))
             (with-current-buffer buffer
               (setq-local smudge-current-page page)
               (pop-to-buffer buffer)
               (smudge-track-search-print items page)
               (message "Track view updated"))
           (message "No more tracks")))))))

(defun smudge-track-album-tracks-update (album page)
  "Fetch PAGE of of tracks for ALBUM."
  (let ((buffer (current-buffer)))
    (smudge-api-album-tracks
     album
     page
     (lambda (json)
       (if-let ((items (smudge-api-get-items json)))
           (with-current-buffer buffer
             (setq-local smudge-current-page page)
             (setq-local smudge-selected-album album)
             (pop-to-buffer buffer)
             (smudge-track-search-print items page)
             (message "Track view updated"))
         (message "No more tracks"))))))

(defun smudge-track-recently-played-tracks-update (page)
  "Fetch PAGE of results for the recently played tracks."
  (let ((buffer (current-buffer)))
    (smudge-api-recently-played
     page
     (lambda (json)
       (if-let ((items (smudge-api-get-playlist-tracks json)))
           (with-current-buffer buffer
             (setq-local smudge-current-page page)
             (setq-local smudge-recently-played t)
             (pop-to-buffer buffer)
             (smudge-track-search-print items page)
             (message "Track view updated"))
         (message "No more tracks"))))))

(defun smudge-track-search-set-list-format ()
  "Configure the column data for the typical track view.
Default to sortin tracks by number when listing the tracks from an album."
  (let* ((base-width (truncate (/ (- (window-width) 30) 3)))
         (default-width (if (bound-and-true-p smudge-selected-album) (+ base-width 4) base-width )))
    (when (not (bound-and-true-p smudge-selected-playlist))
      (setq tabulated-list-sort-key `("#" . nil)))
    (setq tabulated-list-format
          (vconcat (vector `("#" 3 ,(lambda (row-1 row-2)
                                      (< (+ (* 100 (smudge-api-get-disc-number (car row-1)))
                                            (smudge-api-get-track-number (car row-1)))
                                         (+ (* 100 (smudge-api-get-disc-number (car row-2)))
                                            (smudge-api-get-track-number (car row-2))))) :right-align t)
                           `("Track Name" ,default-width t)
                           `("Artist" ,default-width t)
                           `("Album" ,default-width t)
                           `("Time" 8 (lambda (row-1 row-2)
                                        (< (smudge-get-track-duration (car row-1))
                                           (smudge-get-track-duration (car row-2))))))
                   (when (not (bound-and-true-p smudge-selected-album))
                     (vector '("Popularity" 14 t)))))))

(defun smudge-track-search-print (songs page)
  "Append SONGS to the PAGE of track view."
  (let (entries)
    (dolist (song songs)
      (when (smudge-api-is-track-playable song)
        (let* ((artist-name (smudge-api-get-track-artist-name song))
               (album (or (smudge-api-get-track-album song) smudge-selected-album))
               (album-name (smudge-api-get-item-name album))
               (album (smudge-api-get-track-album song)))
          (push (list song
                      (vector (number-to-string (smudge-api-get-track-number song))
                              (smudge-api-get-item-name song)
                              (cons artist-name
                                    (list 'face 'link
                                          'follow-link t
                                          'action `(lambda (_) (smudge-track-search ,(format "artist:\"%s\"" artist-name)))
                                          'help-echo (format "Show %s's tracks" artist-name)
					                                'artist-or-album 'artist))
                              (cons album-name
                                    (list 'face 'link
                                          'follow-link t
                                          'action `(lambda (_) (smudge-track-album-tracks ,album))
                                          'help-echo (format "Show %s's tracks" album-name)
					                                'artist-or-album 'album))
                              (smudge-api-get-track-duration-formatted song)
                              (when (not (bound-and-true-p smudge-selected-album))
                                (smudge-api-popularity-bar (smudge-api-get-track-popularity song)))))
                entries))))
    (smudge-track-search-set-list-format)
    (when (eq 1 page) (setq-local tabulated-list-entries nil))
    (setq-local tabulated-list-entries (append tabulated-list-entries (nreverse entries)))
    (tabulated-list-init-header)
    (tabulated-list-print t)))

(defun smudge-track-album-tracks (album)
  "Open a new buffer that lists the tracks from ALBUM."
  (let ((buffer (get-buffer-create (format "*Album: %s*" (smudge-api-get-item-name album)))))
    (with-current-buffer buffer
      (smudge-track-search-mode)
      (smudge-track-album-tracks-update album 1))))

(defun smudge-track-select-playlist (callback)
  "Call CALLBACK with results of user playlist selection."
  (interactive)
  (smudge-api-current-user
   (lambda (user)
     (smudge-api-user-playlists
      (smudge-api-get-item-id user)
      1
      (lambda (json)
        (if-let* ((choices (mapcar (lambda (a)
                                     (list (smudge-api-get-item-name a) (smudge-api-get-item-id a)))
                                   (smudge-api-get-items json)))
                  (selected (completing-read "Select Playlist: " choices)))
            (when (not (string= "" selected))
              (funcall callback (cadr (assoc selected choices))))))))))

(defun smudge-track-add ()
  "Add the track under the cursor on a playlist.  Prompt for the playlist."
  (interactive)
  (let ((selected-track (tabulated-list-get-id)))
    (smudge-track-select-playlist
     (lambda (playlist)
       (smudge-api-current-user
        (lambda (user)
          (smudge-api-playlist-add-track
           (smudge-api-get-item-id user)
           playlist
           (smudge-api-get-item-uri selected-track)
           (lambda (_)
             (message "Song added.")))))))))

(provide 'smudge-track)
;;; smudge-track.el ends here
