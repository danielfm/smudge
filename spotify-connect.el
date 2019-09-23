;;; package --- Summary

;;; Commentary:

;; spotify-connect.el --- Spotify.el transport for the Spotify Connect API

;; Copyright (C) 2019 Jason Dufair

;;; Code:

(require 'spotify-api)

(defun spotify-connect-player-status ()
  "Get the player status of the currently playing device, if any.
Returns a JSON string in the format:
{
  \"artist\": \"Aesop Rock\",
  \"duration\": 265333,
  \"track_number\": 9,
  \"name\":  \"Shrunk\",
  \"player_state\": \"playing\",
  \"player_shuffling\": \"t\",
  \"player_repeating\": \"context\"
}"
  (let* ((status (spotify-api-get-player-status))
         (track (when status (gethash 'item status)))
         (json (when status
                 (concat
                  "{"
                  (format "\"artist\":\"%s\"," (gethash 'name (car (gethash 'artists track))))
                  (format "\"duration\": %d," (gethash 'duration_ms track))
                  (format "\"track_number\":%d," (gethash 'track_number track))
                  (format "\"name\":\"%s\"," (gethash 'name track))
                  (format "\"player_state\":\"%s\","
                          (if (gethash 'is_playing status) "playing", "paused"))
                  (format "\"player_shuffling\":%s,"
                          (if (not (eq (gethash 'shuffle_state status) :json-false))
                              "true" "false"))
                  (format "\"player_repeating\":%s"
                          (if (string= (gethash 'repeat_state status) "off") "false" "true"))
                  "}"))))
    (when status
      (spotify-replace-mode-line-flags json))))

(defun spotify-connect-get-device-id (player-status)
  "Get the id if from PLAYER-STATUS of the currently playing device, if any."
  (if player-status
      (gethash 'id (gethash 'device player-status))))

(defun spotify-connect-player-play-track (uri &optional context)
  "Play a track URI via Spotify Connect in an optional CONTEXT."
  (spotify-api-play uri context))

(defun spotify-connect-player-pause ()
  "Pause the currently playing track."
  (spotify-api-pause))

(defun spotify-connect-player-toggle-play ()
  "Toggle playing status of current track."
  (let ((player-status (spotify-api-get-player-status)))
    (if player-status
        (if (not (eq (gethash 'is_playing player-status) :json-false))
            (spotify-api-pause)
          (spotify-api-play)))))

(defun spotify-connect-player-next-track ()
  "Skip to the next track."
  (spotify-api-next))

(defun spotify-connect-player-previous-track ()
  "Skip to the previous track."
  (spotify-api-previous))

(defun spotify-connect-get-volume (player-status)
  "Get the volume from PLAYER-STATUS of the currently playing device, if any."
  (if player-status
      (gethash 'volume_percent (gethash 'device player-status))))

(defun spotify-connect-volume-up ()
  "Turn up the volume on the actively playing device."
  (let ((player-status (spotify-api-get-player-status)))
    (spotify-api-set-volume
     (spotify-connect-get-device-id player-status)
     (min (+ (spotify-connect-get-volume player-status) 10) 100)))
  (message "volume increased"))

(defun spotify-connect-volume-down ()
  "Turn down the volume (for what?) on the actively playing device."
  (let ((player-status (spotify-api-get-player-status)))
    (spotify-api-set-volume
     (spotify-connect-get-device-id player-status)
     (max (- (spotify-connect-get-volume player-status) 10) 0)))
  (message "volume decreased"))

(provide 'spotify-connect)

;;; spotify-connect.el ends here
