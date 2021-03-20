;;; smudge-dbus --- Dbus-specific code for Smudge  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Daniel Fernandes Martins

;;; Commentary:

;; This library handles controlling Spotify via the D-Bus interface.  It implements a set of
;; multimethod-like functions that are dispatched in smudge-controller.el.

;; Somehow shuffling, setting volume and loop status do not work as expected.  Querying the
;; attribute does not return the expected value and setting it has no effect.  The dbus interface of
;; Spotify seems to be broken.

;;; Code:

(require 'dbus)
(require 'smudge-controller)

(defun smudge-dbus-call (method &rest args)
  "Call METHOD with optional ARGS via D-Bus on the Spotify service."
  (apply 'dbus-call-method-asynchronously
         :session
         "org.mpris.MediaPlayer2.spotify"
         "/org/mpris/MediaPlayer2"
         "org.mpris.MediaPlayer2.Player"
         method
         nil args))

(defun smudge-dbus-get-property (property)
  "Get value of PROPERTY via D-Bus on the Spotify service."
  (dbus-get-property :session
                     "org.mpris.MediaPlayer2.spotify"
                     "/org/mpris/MediaPlayer2"
                     "org.mpris.MediaPlayer2.Player"
                     property))

(defun smudge-dbus-set-property (property value)
  "Set PROPERTY to VALUE via D-Bus on the Spotify service."
  (dbus-set-property :session
                     "org.mpris.MediaPlayer2.spotify"
                     "/org/mpris/MediaPlayer2"
                     "org.mpris.MediaPlayer2.Player"
                     property
                     value))

(defun smudge-dbus-player-status ()
  "Update the player status to display the current Spotify player status."
  (let* ((metadata (smudge-dbus-get-property "Metadata"))
         (playback-status (smudge-dbus-get-property "PlaybackStatus"))
         (player-state (if playback-status (downcase playback-status) "stopped")))
    (if metadata
        (let ((artist        (car (car (car (cdr (assoc "xesam:artist" metadata))))))
              (name          (car (car (cdr (assoc "xesam:title" metadata)))))
              (track-number  (car (car (cdr (assoc "xesam:trackNumber" metadata)))))
              (duration      (/ (car (car (cdr (assoc "mpris:length" metadata)))) 1000)))
          (if (> track-number 0)
              (smudge-controller-update-metadata
               (concat "{"
                       " \"artist\": \"" artist "\""
                       ",\"duration\": " (number-to-string duration) ""
                       ",\"track_number\": " (number-to-string track-number) ""
                       ",\"name\": \"" name "\""
                       ",\"player_state\": \"" player-state "\""
                       ",\"player_shuffling\": \"-\""
                       ",\"player_repeating\": \"-\""
                       "}"))
            (smudge-controller-update-player-status "")))
      (smudge-controller-update-player-status ""))))

(defun smudge-dbus-player-toggle-play ()
  "Toggle Play/Pause."
  (smudge-dbus-call "PlayPause"))

(defun smudge-dbus-player-next-track ()
  "Play next track."
  (smudge-dbus-call "Next"))

(defun smudge-dbus-player-previous-track ()
  "Play previous previous."
  (smudge-dbus-call "Previous"))

(defun smudge-dbus-volume-up ()
  "Send message about inability to change volume."
  (message "Changing the volume not supported by the Spotify DBus client"))

(defun smudge-dbus-volume-down ()
  "Send message about inability to change volume."
  (message "Changing the volume not supported by the Spotify DBus client"))

(defun smudge-dbus-volume-mute-unmute ()
  "Send message about inability to change volume."
  (message "Changing the volume not supported by the Spotify DBus client"))

(defun smudge-dbus-toggle-repeat ()
  "Dispatch repeat command."
  (message "Toggling repeat status not supported by the Spotify client"))

(defun smudge-dbus-toggle-shuffle ()
  "Dispatch shuffle command."
  (message "Toggline shuffle status not supported by the Spotify client"))

;; TODO: Synchronize this to work the same way as the apple version, if possible
(defun smudge-dbus-player-play-track (track-id context-id)
  "Dispatch message about playing TRACK-ID in CONTEXT-ID."
  (when track-id (smudge-dbus-call "Pause"))
  (run-at-time "1 sec" nil 'smudge-dbus-call "OpenUri" (or track-id context-id)))

(provide 'smudge-dbus)
;;; smudge-dbus.el ends here
