;;; spotify-dbus --- Dbus-specific code for Spotify.el

;; Copyright (C) 2014-2016 Daniel Fernandes Martins

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
  (let ((metadata (spotify-dbus-get-property "Metadata")))
    (if (and (spotify-connected-p) metadata)
        (let ((track-id (car (car (cdr (assoc "mpris:trackid" metadata)))))
              (artist   (car (car (car (cdr (assoc "xesam:artist" metadata))))))
              (title    (car (car (cdr (assoc "xesam:title" metadata)))))
              (track-n  (car (car (cdr (assoc "xesam:trackNumber" metadata)))))
              (disc-n   (car (car (cdr (assoc "xesam:discNumber" metadata)))))
              (length   (/ (car (car (cdr (assoc "mpris:length" metadata)))) 1000)))
          (if (> track-n 0)
              (spotify-replace-mode-line-flags
               (concat track-id "\n"
                       artist   "\n"
                       title    "\n"
                       (number-to-string track-n) "\n"
                       (number-to-string disc-n)  "\n"
                       (number-to-string length)  "\n"
                       "-\n" ;; TODO: unable to get the player state via D-Bus
                       "-\n" ;; TODO: unable to get the player position via D-Bus
                       ))
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

;; TODO: Currently not supported by the Spotify client D-Bus interface
(defun spotify-dbus-toggle-repeat ()
  (message "Toggling repeat status not supported by the Spotify client"))

;; TODO: Currently not supported by the Spotify client D-Bus interface
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
