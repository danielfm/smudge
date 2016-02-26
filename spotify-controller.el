;; spotify-controller.el --- TODO

;; Copyright (C) 2014-2016 Daniel Fernandes Martins

;; Code:

;;; TODO: D-Bus support not implemented
(defcustom spotify-transport 'apple
  "How the commands should be sent to Spotify process."
  :type '(choice (symbol :tag "AppleScript" apple)
                 (symbol :tag "D-Bus" dbus)))

(defcustom spotify-mode-line-refresh-interval 1
  "The interval, in seconds, that the mode line must be updated. Set to 0 to
   disable this feature."
  :type 'integer)

(defcustom spotify-mode-line-format "%a - %t (%s)"
  "Format used to display the current Spotify client player status. The
following placeholders are supported:

* %u - Track URI (i.e. spotify:track:<ID>)
* %a - Artist name
* %t - Track name
* %n - Track #
* %d - Track disc #
* %s - Player state (i.e. playing, paused, stopped)
* %l - Track duration, in minutes (i.e. 01:35)
* %p - Player position in current track, in minutes (i.e. 01:35)")

(defvar spotify-timer nil)

;; simple facility to emulate multimethods
(defun spotify-apply (suffix &rest args)
  (let ((func-name (format "spotify-%s-%s" spotify-transport suffix)))
    (apply (intern func-name) args)))

(defun spotify-set-mode-line (process output)
  "Sets the output of the player status process to the mode line."
  (let ((mode-line spotify-mode-line-format)
        (fields (split-string output "\n"))
        (duration-format "%m:%02s"))
    (setq mode-line (replace-regexp-in-string "%u" (first fields) mode-line))
    (setq mode-line (replace-regexp-in-string "%a" (second fields) mode-line))
    (setq mode-line (replace-regexp-in-string "%t" (third fields) mode-line))
    (setq mode-line (replace-regexp-in-string "%n" (fourth fields) mode-line))
    (setq mode-line (replace-regexp-in-string "%d" (fifth fields) mode-line))
    (setq mode-line (replace-regexp-in-string "%s" (seventh fields) mode-line))
    (setq mode-line (replace-regexp-in-string "%l" (format-seconds duration-format (/ (string-to-number (sixth fields)) 1000)) mode-line))
    (setq mode-line (replace-regexp-in-string "%p" (format-seconds duration-format (string-to-number (eighth fields))) mode-line))
    (spotify-update-mode-line mode-line)
    (with-current-buffer (process-buffer process)
      (delete-region (point-min) (point-max)))))

(defun start-mode-line-timer ()
  "Starts the timer that updates the mode line according to the Spotify
   player status."
  (stop-mode-line-timer)
  (when (> spotify-mode-line-refresh-interval 0)
    (let ((first-run (format "%d sec" spotify-mode-line-refresh-interval))
          (interval spotify-mode-line-refresh-interval))
      (setq spotify-timer
            (run-at-time first-run interval 'spotify-refresh-mode-line)))))

(defun stop-mode-line-timer ()
  "Stops the timer that updates the mode line."
  (when (and (boundp 'spotify-timer) (timerp spotify-timer))
    (cancel-timer spotify-timer)
    (spotify-player-status)))

(defun spotify-player-status ()
  "Updates the mode line to display the current Spotify player status."
  (interactive)
  (spotify-apply "player-status"))

(defun spotify-refresh-mode-line (&rest args)
  "Starts the player status process in order to update the mode line."
  (spotify-apply "player-status"))

(defun spotify-play-track (track-id context-id)
  "Sends a `play' command to Spotify process passing a context id."
  (interactive)
  (spotify-apply "player-play-track" track-id context-id))

(defun spotify-toggle-play ()
  "Sends a `playpause' command to Spotify process."
  (interactive)
  (spotify-apply "player-toggle-play"))

(defun spotify-play ()
  "Sends a `play' command to Spotify process."
  (interactive)
  (spotify-apply "player-play"))

(defun spotify-next-track ()
  "Sends a `next track' command to Spotify process."
  (interactive)
  (spotify-apply "player-next-track"))

(defun spotify-previous-track ()
  "Sends a `previous track' command to Spotify process."
  (interactive)
  (spotify-apply "player-previous-track"))

(defun spotify-pause ()
  "Sends a `pause' command to Spotify process."
  (interactive)
  (spotify-apply "player-pause"))

(defun spotify-playing-p ()
  "Returns whether Spotify is playing."
  (interactive)
  (spotify-apply "player-playing-p"))

(defun spotify-repeating-p ()
  "Returns whether Spotify have repeating turned on."
  (interactive)
  (spotify-apply "repeating-p"))

(defun spotify-toggle-repeat ()
  "Sends a command to Spotify process to toggle the repeating flag."
  (interactive)
  (spotify-apply "toggle-repeat"))

(defun spotify-shuffling-p ()
  "Returns whether Spotify have shuffling turned on."
  (interactive)
  (spotify-apply "shuffling-p"))

(defun spotify-toggle-shuffle ()
  "Sends a command to Spotify process to toggle the shuffling flag."
  (interactive)
  (spotify-apply "toggle-shuffle"))

(defun spotify-current-track-artist ()
  "Retrieves the artist name of the track being played in Spotify app."
  (interactive)
  (spotify-apply "current-track-artist"))

(defun spotify-current-track-album ()
  "Retrieves the album name of the track being played in Spotify app."
  (interactive)
  (spotify-apply "current-track-album"))

(defun spotify-current-track-name ()
  "Retrieves the name of the track being played in Spotify app."
  (interactive)
  (spotify-apply "current-track-name"))

(provide 'spotify-controller)
