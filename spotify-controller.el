;; spotify-controller.el --- Generic player controller interface for Spotify.el

;; Copyright (C) 2014-2016 Daniel Fernandes Martins

;; Code:

(defmacro if-gnu-linux (then else)
  "Evaluates `then' form if Emacs is running in GNU/Linux, otherwise evaluates
`else' form."
  `(if (eq system-type 'gnu/linux) ,then ,else))

(defmacro when-gnu-linux (then)
  "Evaluates `then' form if Emacs is running in GNU/Linux."
  `(if-gnu-linux ,then nil))

(defmacro if-darwin (then else)
  "Evaluates `then' form if Emacs is running in OS X, otherwise evaluates
`else' form."
  `(if (eq system-type 'darwin) ,then ,else))

(defmacro when-darwin (then)
  "Evaluates `then' form if Emacs is running in OS X."
  `(if-darwin ,then nil))

(defcustom spotify-transport (if-gnu-linux 'dbus 'apple)
  "How the commands should be sent to Spotify process. Defaults for `dbus' for
GNU/Linux, `apple' otherwise."
  :type '(choice (symbol :tag "AppleScript" apple)
                 (symbol :tag "D-Bus" dbus)))

;; TODO: No modeline support for linux just yet
(defcustom spotify-mode-line-refresh-interval (if-gnu-linux 0 1)
  "The interval, in seconds, that the mode line must be updated. Set to 0 to
   disable this feature."
  :type 'integer)

(defcustom spotify-mode-line-truncate-length 15
  "The maximum number of characters to truncated fields in
`spotify-mode-line-format'.")

(defcustom spotify-mode-line-format "%at - %t [%l]"
  "Format used to display the current Spotify client player status. The
following placeholders are supported:

* %u  - Track URI (i.e. spotify:track:<ID>)
* %a  - Artist name
* %at - Artist name (truncated)
* %t  - Track name
* %tt - Track name (truncated)
* %n  - Track #
* %d  - Track disc #
* %s  - Player state (i.e. playing, paused, stopped)
* %l  - Track duration, in minutes (i.e. 01:35)
* %p  - Player position in current track, in minutes (i.e. 01:35)
")

(defvar spotify-timer nil)

;; simple facility to emulate multimethods
(defun spotify-apply (suffix &rest args)
  (let ((func-name (format "spotify-%s-%s" spotify-transport suffix)))
    (apply (intern func-name) args)))

(defun spotify-replace-mode-line-flags (metadata)
  ""
  (let ((mode-line spotify-mode-line-format)
        (fields (split-string metadata "\n"))
        (duration-format "%m:%02s"))
    (if (or (< (length fields) 8)
            (string= "stopped" (seventh fields)))
        (setq mode-line "")
      (progn
        (setq mode-line (replace-regexp-in-string "%u" (first fields) mode-line))
        (setq mode-line (replace-regexp-in-string "%at" (truncate-string-to-width (second fields) spotify-mode-line-truncate-length 0 nil "...") mode-line))
        (setq mode-line (replace-regexp-in-string "%a" (second fields) mode-line))
        (setq mode-line (replace-regexp-in-string "%tt" (truncate-string-to-width (third fields) spotify-mode-line-truncate-length 0 nil "...") mode-line))
        (setq mode-line (replace-regexp-in-string "%t" (third fields) mode-line))
        (setq mode-line (replace-regexp-in-string "%n" (fourth fields) mode-line))
        (setq mode-line (replace-regexp-in-string "%d" (fifth fields) mode-line))
        (setq mode-line (replace-regexp-in-string "%s" (seventh fields) mode-line))
        (setq mode-line (replace-regexp-in-string "%l" (format-seconds duration-format (/ (string-to-number (sixth fields)) 1000)) mode-line))
        (setq mode-line (replace-regexp-in-string "%p" (format-seconds duration-format (string-to-number (eighth fields))) mode-line))))
    (spotify-update-mode-line mode-line)))

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

(defun spotify-play-uri (uri)
  "Sends a `play' command to Spotify process passing the given URI."
  (interactive "SSpotify URI: ")
  (spotify-apply "player-play-track" uri nil))

(defun spotify-play-track (track &optional context)
  "Sends a `play' command to Spotify process passing a context id."
  (interactive)
  (spotify-apply "player-play-track"
                 (when track (spotify-get-item-uri track))
                 (when context (spotify-get-item-uri context))))

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

(defun spotify-toggle-repeat ()
  "Sends a command to Spotify process to toggle the repeating flag."
  (interactive)
  (spotify-apply "toggle-repeat"))

(defun spotify-toggle-shuffle ()
  "Sends a command to Spotify process to toggle the shuffling flag."
  (interactive)
  (spotify-apply "toggle-shuffle"))

(provide 'spotify-controller)
