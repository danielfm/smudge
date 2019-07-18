;; spotify-web.el --- Web-specific code for Spotify.el

;; Copyright (C) 2019 Tatu Lahtela

;; Code:

;; Write script to a temp file
(setq spotify-web-player-status-script-file
      (make-temp-file "spotify.el" nil nil spotify-web-player-status-script))

(set (make-local-variable 'spotify-web-player-status-json) nil)

(defun spotify-web-player-status ()
  "Updates the mode line to display the current Spotify player status."
  (let ((json (spotify-api-player-status)))
    (if json
      (let ((artist (gethash 'name (first (gethash 'artists (gethash 'item json)))))
	    (duration (gethash 'duration_ms (gethash 'item json)))
	    (track-number (gethash 'track_number (gethash 'item json)))
	    (player-shuffling (if (equal json-false(gethash 'shuffle_state json)) "false" "true"))
	    (player-state (if (equal json-false(gethash 'is_playing json)) "stopped" "playing"))
	    (name (gethash 'name (gethash 'item json)))
	    (repeat-state (if (equal "off" (gethash 'repeat_state json)) "false" "true")))
	(if artist
	    (spotify-replace-mode-line-flags
	     (concat "{"
		     " \"artist\": \"" artist "\""
		     ",\"duration\": " (number-to-string duration) ""
		     ",\"track_number\": " (number-to-string track-number) ""
		     ",\"name\": \"" name "\""
		     ",\"player_state\": \"" player-state "\""
		     ",\"player_shuffling\": \"" player-shuffling "\""
		     ",\"player_repeating\": \"" repeat-state "\""
		     "}")))))
  (setq spotify-web-player-status-json json)))

(defun spotify-web-player-state ()
  )

(defun spotify-web-player-toggle-play ()
  (spotify-api-player-play))

(defun spotify-web-player-next-track ()
  (spotify-api-player-next)
  (spotify-web-player-status))

(defun spotify-web-player-previous-track ()
  (spotify-api-previous))

(defun spotify-web-is-shuffling-supported ()
  nil)

(defun spotify-web-is-repeating-supported ()
  nil)

(defun spotify-web-toggle-repeat ())

(defun spotify-web-toggle-shuffle ())

(defun spotify-web-is-repeating ()
  nil)

(defun spotify-web-is-shuffling ()
  nil)

(defun spotify-web-get-device-id ()
  (if spotify-web-player-status-json
      (gethash 'id (gethash 'device spotify-web-player-status-json))))

(defun spotify-web-player-play-track (track-id context-id)
  (spotify-api-player-play-track track-id context-id))

(defun spotify-web-player-play ()
  (spotify-api-player-play))

(defun spotify-web-player-pause ()
  (spotify-api-player-pause))

(provide 'spotify-web)
