;; spotify-apple.el --- Apple-specific code for Spotify.el

;; Copyright (C) 2014 Daniel Fernandes Martins

;; Code:

(defcustom spotify-osascript-bin-path "/usr/bin/osascript"
  "Path to `osascript' binary."
  :type 'string)

(defun spotify-apple-command-true-output-p (out-str)
  (string= "true" out-str))

(defun spotify-apple-command (cmd)
  (replace-regexp-in-string
   "\n$" ""
   (shell-command-to-string  
    (format "%s -e 'tell application \"Spotify\" to %s'" spotify-osascript-bin-path cmd))))

(defun spotify-apple-player-state ()
  (spotify-apple-command "get player state"))

(defun spotify-apple-player-toggle-play ()
  (spotify-apple-command "playpause"))

(defun spotify-apple-player-next-track ()
  (spotify-apple-command "next track"))

(defun spotify-apple-player-previous-track ()
  (spotify-apple-command "previous track"))

(defun spotify-apple-toggle-repeat ()
  (spotify-apple-command "set repeating to not repeating"))

(defun spotify-apple-toggle-shuffle ()
  (spotify-apple-command "set shuffling to not shuffling"))

(defun spotify-apple-player-play-track (context-id)
  (spotify-apple-command (format "play track \"%s\"" context-id)))

(defun spotify-apple-repeating-p ()
  (spotify-apple-command-true-output-p
   (spotify-apple-command "get repeating")))

(defun spotify-apple-shuffling-p ()
  (spotify-apple-command-true-output-p
   (spotify-apple-command "get shuffling")))

(defun spotify-apple-player-pause ()
  (spotify-apple-command "pause"))

(defun spotify-apple-player-playing-p ()
  (string= "playing" (spotify-apple-player-state)))

(defun spotify-apple-current-track-artist ()
  (spotify-apple-command "artist of current track"))

(defun spotify-apple-current-track-album ()
  (spotify-apple-command "album of current track"))

(defun spotify-apple-current-track-name ()
  (spotify-apple-command "name of current track"))

(provide 'spotify-apple)
