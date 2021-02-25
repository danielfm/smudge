;;; spotify-client-dbus --- Dbus-specific code for spotify-client  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Daniel Fernandes Martins

;;; Commentary:

;; This library handles controlling Spotify via the D-Bus interface.  It implements a set of
;; multimethod-like functions that are dispatched in spotify-client-controller.el.

;; Somehow shuffling, setting volume and loop status do not work as expected.  Querying the
;; attribute does not return the expected value and setting it has no effect.  The dbus interface of
;; Spotify seems to be broken.

;;; Code:

(require 'dbus)
(require 'spotify-client-controller)

(defun spotify-client-dbus-call (method &rest args)
  "Call METHOD with optional ARGS via D-Bus on the Spotify service."
  (apply 'dbus-call-method-asynchronously
         :session
         "org.mpris.MediaPlayer2.spotify"
         "/org/mpris/MediaPlayer2"
         "org.mpris.MediaPlayer2.Player"
         method
         nil args))

(defun spotify-client-dbus-get-property (property)
  "Get value of PROPERTY via D-Bus on the Spotify service."
  (dbus-get-property :session
                     "org.mpris.MediaPlayer2.spotify"
                     "/org/mpris/MediaPlayer2"
                     "org.mpris.MediaPlayer2.Player"
                     property))

(defun spotify-client-dbus-set-property (property value)
  "Set PROPERTY to VALUE via D-Bus on the Spotify service."
  (dbus-set-property :session
                     "org.mpris.MediaPlayer2.spotify"
                     "/org/mpris/MediaPlayer2"
                     "org.mpris.MediaPlayer2.Player"
                     property
                     value))

(defun spotify-client-dbus-player-status ()
  "Update the player status to display the current Spotify player status."
  (let* ((metadata (spotify-client-dbus-get-property "Metadata"))
         (playback-status (spotify-client-dbus-get-property "PlaybackStatus"))
         (player-state (if playback-status (downcase playback-status) "stopped")))
    (if metadata
        (let ((artist        (car (car (car (cdr (assoc "xesam:artist" metadata))))))
              (name          (car (car (cdr (assoc "xesam:title" metadata)))))
              (track-number  (car (car (cdr (assoc "xesam:trackNumber" metadata)))))
              (duration      (/ (car (car (cdr (assoc "mpris:length" metadata)))) 1000)))
          (if (> track-number 0)
              (spotify-client-controller-update-metadata
               (concat "{"
                       " \"artist\": \"" artist "\""
                       ",\"duration\": " (number-to-string duration) ""
                       ",\"track_number\": " (number-to-string track-number) ""
                       ",\"name\": \"" name "\""
                       ",\"player_state\": \"" player-state "\""
                       ",\"player_shuffling\": \"-\""
                       ",\"player_repeating\": \"-\""
                       "}"))
            (spotify-client-controller-update-player-status "")))
      (spotify-client-controller-update-player-status ""))))

(defun spotify-client-dbus-player-toggle-play ()
  "Toggle Play/Pause."
  (spotify-client-dbus-call "PlayPause"))

(defun spotify-client-dbus-player-next-track ()
  "Play next track."
  (spotify-client-dbus-call "Next"))

(defun spotify-client-dbus-player-previous-track ()
  "Play previous previous."
  (spotify-client-dbus-call "Previous"))

(defun spotify-client-dbus-volume-up ()
  "Send message about inability to change volume."
  (message "Changing the volume not supported by the Spotify DBus client"))

(defun spotify-client-dbus-volume-down ()
  "Send message about inability to change volume."
  (message "Changing the volume not supported by the Spotify DBus client"))

(defun spotify-client-dbus-volume-mute-unmute ()
  "Send message about inability to change volume."
  (message "Changing the volume not supported by the Spotify DBus client"))

(defun spotify-client-dbus-toggle-repeat ()
  "Dispatch repeat command."
  (message "Toggling repeat status not supported by the Spotify client"))

(defun spotify-client-dbus-toggle-shuffle ()
  "Dispatch shuffle command."
  (message "Toggline shuffle status not supported by the Spotify client"))

;; TODO: Synchronize this to work the same way as the apple version, if possible
(defun spotify-client-dbus-player-play-track (track-id context-id)
  "Dispatch message about playing TRACK-ID in CONTEXT-ID."
  (when track-id (spotify-client-dbus-call "Pause"))
  (run-at-time "1 sec" nil 'spotify-client-dbus-call "OpenUri" (or track-id context-id)))

(provide 'spotify-client-dbus)
;;; spotify-client-dbus.el ends here
