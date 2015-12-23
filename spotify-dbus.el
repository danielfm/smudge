;;; spotify-dbus --- Dbus-specific code for Spotify.el

;;; Commentary:

;; Somehow shuffeling, setting volume and loop status work not as expected.
;; Quering the attribute does not return the expected value and setting it
;; has no effect. I'm not sure if it is not the spotify client.

;;; Code:
(defun spotify-dbus-call (method)
    "Call METHOD via D-Bus on the Spotify service."
    (dbus-call-method-asynchronously :session
				     "org.mpris.MediaPlayer2.spotify"
				     "/org/mpris/MediaPlayer2"
				     "org.mpris.MediaPlayer2.Player"
				     method
				     nil))

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

(defun spotify-dbus-player-toggle-play ()
  "Toggle Play/Pause."
  (spotify-dbus-call "PlayPause"))

(defun spotify-dbus-player-next-track ()
  "Play next track."
  (spotify-dbus-call "Next"))

(defun spotify-dbus-player-previous-track ()
  "Play previous previous."
  (spotify-dbus-call "Previous"))

(defun spotify-dbus-toggle-repeat ()
  "Toggle loop options."
  (if (spotify-dbus-repeating-p)
      (spotify-dbus-set-property "LoopStatus" nil)
    (spotify-dbus-set-property "LoopStatus" "Playlist")))

(defun spotify-dbus-toggle-shuffle ()
  "Toggle shuffle."
  (if (spotify-dbus-shuffling-p)
      (spotify-dbus-set-property "Shuffle" nil)
    (spotify-dbus-set-property "Shuffle" t)))

(defun spotify-dbus-player-play-track (context-id)
  "Play the given CONTEXT-ID."
  (spotify-dbus-call (format "OpenUri \"string:%s\"" context-id)))

(defun spotify-dbus-repeating-p ()
  "Check if repeating is on."
  (string= "Playlist"
   (spotify-dbus-get-property "LoopStatus")))

(defun spotify-dbus-shuffling-p ()
  "Check if shuffeling is on."
   (spotify-dbus-get-property "Shuffle"))

(defun spotify-dbus-player-pause ()
  "Pause playback."
  (spotify-dbus-call "Pause"))

(defun spotify-dbus-player-playing-p ()
  "Check if Playing."
  (string= "Playing"
	   (spotify-dbus-get-property "PlaybackStatus")))

(defun spotify-dbus-current-track-artist ()
  "Return the artist which is currently playing."
  (car (car (car (cdr (assoc "xesam:artist" (spotify-dbus-get-property "Metadata")))))))

(defun spotify-apple-current-track-album ()
  "Return the album which is currently playing."
  (car (car (cdr (assoc "xesam:album" (spotify-dbus-get-property "Metadata"))))))

(defun spotify-apple-current-track-name ()
  "Return the current track name."
  (car (car (cdr (assoc "xesam:title" (spotify-dbus-get-property "Metadata"))))))

(provide 'spotify-dbus)
;;; spotify-dbus ends here
