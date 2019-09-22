;;; package --- Summary

;;; Commentary:

;; spotify-connect.el --- Spotify.el transport for the Spotify Connect API

;; Copyright (C) 2019 Jason Dufair

;;; Code:

(require 'spotify-api)

(defun spotify-connect-player-status ()
  "Get the player status of the currently playing device, if any."
  (spotify-api-get-player-status))

(defun spotify-connect-get-device-id (player-status)
  "Get the id if from PLAYER-STATUS of the currently playing device, if any."
  (if player-status
      (gethash 'id (gethash 'device player-status))))

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
