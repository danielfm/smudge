;;; spotify-dbus --- Dbus-specific code for Spotify.el

;; Copyright (C) 2014-2019 Daniel Fernandes Martins

;;; Commentary:

;; Somehow shuffeling, setting volume and loop status work not as expected.
;; Querying the attribute does not return the expected value and setting it
;; has no effect.
;; The dbus interface of spotify seems to be broken.

;;; Code:

(require 'dbus)

(defun spotify-dbus-call (method &rest args)
  "Call METHOD with optional ARGS via D-Bus on the Spotify service."
  (apply 'dbus-call-method-asynchronously
         :session
         "org.mpris.MediaPlayer2.spotify"
         "/org/mpris/MediaPlayer2"
         "org.mpris.MediaPlayer2.Player"
         method
         nil args))

(defun spotify-dbus-get-property (property)
  "Get value of PROPERTY via D-Bus on the Spotify service."
  (dbus-get-property :session
                     "org.mpris.MediaPlayer2.spotify"
                     "/org/mpris/MediaPlayer2"
                     "org.mpris.MediaPlayer2.Player"
                     property))

(defun spotify-dbus-set-property (property value)
  "Set PROPERTY to VALUE via D-Bus on the Spotify service."
  (dbus-set-property :session
                     "org.mpris.MediaPlayer2.spotify"
                     "/org/mpris/MediaPlayer2"
                     "org.mpris.MediaPlayer2.Player"
                     property
                     value))

(defun spotify-dbus-player-status ()
  "Updates the mode line to display the current Spotify player status."
  (let* ((metadata (spotify-dbus-get-property "Metadata"))
         (playback-status (spotify-dbus-get-property "PlaybackStatus"))
         (player-state (if playback-status (downcase playback-status) "stopped")))
    (if metadata
        (let ((artist        (car (car (car (cdr (assoc "xesam:artist" metadata))))))
              (name          (car (car (cdr (assoc "xesam:title" metadata)))))
              (track-number  (car (car (cdr (assoc "xesam:trackNumber" metadata)))))
              (duration      (/ (car (car (cdr (assoc "mpris:length" metadata)))) 1000)))
          (if (> track-number 0)
              (spotify-replace-mode-line-flags
               (concat "{"
                       " \"artist\": \"" artist "\""
                       ",\"duration\": " (number-to-string duration) ""
                       ",\"track_number\": " (number-to-string track-number) ""
                       ",\"name\": \"" name "\""
                       ",\"player_state\": \"" player-state "\""
                       ",\"player_shuffling\": \"-\""
                       ",\"player_repeating\": \"-\""
                       "}"))
            (spotify-update-mode-line "")))
      (spotify-update-mode-line ""))))

(defun spotify-dbus-player-toggle-play ()
  "Toggle Play/Pause."
  (spotify-dbus-call "PlayPause"))

(defun spotify-dbus-player-next-track ()
  "Play next track."
  (spotify-dbus-call "Next"))

(defun spotify-dbus-player-previous-track ()
  "Play previous previous."
  (spotify-dbus-call "Previous"))

(defun spotify-dbus-volume-up ()
  (message "Changing the volume not supported by the Spotify DBus client"))

(defun spotify-dbus-volume-down ()
  (message "Changing the volume not supported by the Spotify DBus client"))

(defun spotify-dbus-volume-mute-unmute ()
  (message "Changing the volume not supported by the Spotify DBus client"))

(defun spotify-dbus-toggle-repeat ()
  (message "Toggling repeat status not supported by the Spotify client"))

(defun spotify-dbus-toggle-shuffle ()
  (message "Toggline shuffle status not supported by the Spotify client"))

;; TODO: Synchronize this to work the same way as the apple version, if possible
(defun spotify-dbus-player-play-track (track-id context-id)
  (when track-id (spotify-dbus-call "Pause"))
  (run-at-time "1 sec" nil 'spotify-dbus-call "OpenUri" (or track-id context-id)))

(defun spotify-dbus-player-play ()
  (spotify-dbus-call "Play"))

(defun spotify-dbus-player-pause ()
  (spotify-dbus-call "Pause"))

(provide 'spotify-dbus)
