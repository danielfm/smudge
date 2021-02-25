;;; spotify-client-connect.el --- Control remote and local Spotify instances  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jason Dufair

;;; Commentary:

;; This library uses the "connect" APIs to control transport functions of remote and local instances
;; of Spotify clients.  It implements a set of multimethod-like functions that are dispatched in
;; spotify-client-controller.el.

;; spotify-client-connect.el --- spotify-client.el transport for the Spotify Connect API

;;; Code:

(require 'spotify-client-api)
(require 'spotify-client-controller)

(defun spotify-client-connect-player-status ()
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
  (spotify-client-api-get-player-status
   (lambda (status)
     (if-let* ((status status)
               (track (gethash 'item status))
               (json (concat
                      "{"
                      (format "\"artist\":\"%s\","
                              (gethash 'name (car (gethash 'artists track))))
                      (format "\"duration\": %d,"
                              (gethash 'duration_ms track))
                      (format "\"track_number\":%d,"
                              (gethash 'track_number track))
                      (format "\"name\":\"%s\","
                              (gethash 'name track))
                      (format "\"player_state\":\"%s\","
                              (if (eq (gethash 'is_playing status) :json-false) "paused" "playing"))
                      (format "\"player_shuffling\":%s,"
                              (if (not (eq (gethash 'shuffle_state status) :json-false))"true" "false"))
                      (format "\"player_repeating\":%s"
                              (if (string= (gethash 'repeat_state status) "off") "false" "true"))
                      "}")))
         (spotify-client-controller-update-metadata json)
       (spotify-client-controller-update-metadata nil)))))

(defmacro spotify-client-connect-when-device-active (body)
  "Evaluate BODY when there is an active device, otherwise show an error message."
  `(spotify-client-api-device-list
    (lambda (json)
      (if-let ((json json)
               (devices (gethash 'devices json))
               (active (> (length (seq-filter (lambda (dev) (eq (gethash 'is_active dev) t)) devices)) 0)))
          (progn ,body)
        (message "No active device")))))

(defun spotify-client-connect-player-play-track (uri &optional context)
  "Play a track URI via Spotify Connect in an optional CONTEXT."
  (spotify-client-connect-when-device-active
   (spotify-client-api-play nil uri context)))

(defun spotify-client-connect-player-pause ()
  "Pause the currently playing track."
  (spotify-client-connect-when-device-active
   (spotify-client-api-pause)))

(defun spotify-client-connect-player-toggle-play ()
  "Toggle playing status of current track."
  (spotify-client-connect-when-device-active
   (spotify-client-api-get-player-status
    (lambda (status)
      (if status
          (if (not (eq (gethash 'is_playing status) :json-false))
              (spotify-client-api-pause)
            (spotify-client-api-play)))))))

(defun spotify-client-connect-player-next-track ()
  "Skip to the next track."
  (spotify-client-connect-when-device-active
   (spotify-client-api-next)))

(defun spotify-client-connect-player-previous-track ()
  "Skip to the previous track."
  (spotify-client-connect-when-device-active
   (spotify-client-api-previous)))

(defun spotify-client-connect-volume-up ()
  "Turn up the volume on the actively playing device."
  (spotify-client-connect-when-device-active
   (spotify-client-api-get-player-status
    (lambda (status)
      (let ((new-volume (min (+ (spotify-client-connect-get-volume status) 10) 100)))
        (spotify-client-api-set-volume
         (spotify-client-connect-get-device-id status)
         new-volume
         (lambda (_)
           (message "Volume increased to %d%%" new-volume))))))))

(defun spotify-client-connect-volume-down ()
  "Turn down the volume (for what?) on the actively playing device."
  (spotify-client-connect-when-device-active
   (spotify-client-api-get-player-status
    (lambda (status)
      (let ((new-volume (max (- (spotify-client-connect-get-volume status) 10) 0)))
        (spotify-client-api-set-volume
         (spotify-client-connect-get-device-id status)
         new-volume
         (lambda (_)
           (message "Volume decreased to %d%%" new-volume))))))))

(defun spotify-client-connect-volume-mute-unmute ()
  "Mute/unmute the volume on the actively playing device by setting the volume to 0."
  (spotify-client-connect-when-device-active
   (spotify-client-api-get-player-status
    (lambda (status)
      (let ((volume (spotify-client-connect-get-volume status)))
        (if (eq volume 0)
            (spotify-client-api-set-volume (spotify-client-connect-get-device-id status) 100
                                    (lambda (_) (message "Volume unmuted")))
          (spotify-client-api-set-volume (spotify-client-connect-get-device-id status) 0
                                  (lambda (_) (message "Volume muted")))))))))

(defun spotify-client-connect-toggle-repeat ()
  "Toggle repeat for the current track."
  (spotify-client-connect-when-device-active
   (spotify-client-api-get-player-status
    (lambda (status)
      (spotify-client-api-repeat (if (spotify-client-connect--is-repeating status) "off" "context"))))))

(defun spotify-client-connect-toggle-shuffle ()
  "Toggle shuffle for the current track."
  (spotify-client-connect-when-device-active
   (spotify-client-api-get-player-status
    (lambda (status)
      (spotify-client-api-shuffle (if (spotify-client-connect--is-shuffling status) "false" "true"))))))

(defun spotify-client-connect-get-device-id (player-status)
  "Get the id if from PLAYER-STATUS of the currently playing device, if any."
  (when player-status
    (gethash 'id (gethash 'device player-status))))

(defun spotify-client-connect-get-volume (player-status)
  "Get the volume from PLAYER-STATUS of the currently playing device, if any."
  (when player-status
    (gethash 'volume_percent (gethash 'device player-status))))

(defun spotify-client-connect--is-shuffling (player-status)
  "Business logic for shuffling state of PLAYER-STATUS."
  (and player-status
         (not (eq (gethash 'shuffle_state player-status) :json-false))))

(defun spotify-client-connect--is-repeating (player-status)
  "Business logic for repeat state of PLAYER-STATUS."
  (string= (gethash 'repeat_state player-status) "context"))


(provide 'spotify-client-connect)

;;; spotify-client-connect.el ends here
