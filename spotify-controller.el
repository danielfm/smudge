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
(defcustom spotify-mode-line-refresh-interval 1
  "The interval, in seconds, that the mode line must be updated. Set to 0 to
   disable this feature."
  :type 'integer)

(defcustom spotify-mode-line-truncate-length 15
  "The maximum number of characters to truncated fields in
`spotify-mode-line-format'.")

(defcustom spotify-mode-line-playing-text "Playing"
  "Text to be displayed when Spotify is playing")

(defcustom spotify-mode-line-paused-text "Paused"
  "Text to be displayed when Spotify is paused")

(defcustom spotify-mode-line-stopped-text "Stopped"
  "Text to be displayed when Spotify is stopped")

(defcustom spotify-mode-line-repeating-text "R"
  "Text to be displayed when repeat is enabled")

(defcustom spotify-mode-line-not-repeating-text "-"
  "Text to be displayed when repeat is disabled")

(defcustom spotify-mode-line-shuffling-text "S"
  "Text to be displayed when shuffling is enabled")

(defcustom spotify-mode-line-not-shuffling-text "-"
  "Text to be displayed when shuffling is disabled")

(defcustom spotify-mode-line-format "[%p: %a - %t ◷ %l %r%s]"
  "Format used to display the current Spotify client player status. The
following placeholders are supported:

* %a - Artist name (truncated)
* %t - Track name (truncated)
* %n - Track #
* %l - Track duration, in minutes (i.e. 01:35)
* %p - Player status indicator for 'playing', 'paused', and 'stopped' states
* %s - Player shuffling status indicator
* %r - Player repeating status indicator")

(defvar spotify-timer nil)

;; simple facility to emulate multimethods
(defun spotify-apply (suffix &rest args)
  (let ((func-name (format "spotify-%s-%s" spotify-transport suffix)))
    (apply (intern func-name) args)))

(defun spotify-mode-line-playing-indicator (str)
  "Returns the value of the player state variable corresponding to the player's
current state (playing, stopped, paused)."
  (cond ((string= "playing" str) spotify-mode-line-playing-text)
        ((string= "stopped" str) spotify-mode-line-stopped-text)
        ((string= "paused" str) spotify-mode-line-paused-text)))

(defun spotify-mode-line-shuffling-indicator (shuffling)
  "Returns the value of the shuffling state variable corresponding to the
current shuffling state."
  (if (eq shuffling t)
      spotify-mode-line-shuffling-text
    spotify-mode-line-not-shuffling-text))

(defun spotify-mode-line-repeating-indicator (repeating)
  "Returns the value of the repeating state variable corresponding to the
current repeating state."
  (if (eq repeating t)
      spotify-mode-line-repeating-text
    spotify-mode-line-not-repeating-text))

(defun spotify-replace-mode-line-flags (metadata)
  "Compose the playing status string to be displayed in the mode-line."
  (let* ((mode-line spotify-mode-line-format)
         (duration-format "%m:%02s")
         (json-object-type 'hash-table)
         (json-key-type 'symbol)
         (json (condition-case nil
                   (json-read-from-string metadata)
                 (error (spotify-update-mode-line "")
                        nil))))
    (when json
      (progn
        (setq mode-line (replace-regexp-in-string "%a" (truncate-string-to-width (gethash 'artist json) spotify-mode-line-truncate-length 0 nil "...") mode-line))
        (setq mode-line (replace-regexp-in-string "%t" (truncate-string-to-width (gethash 'name json) spotify-mode-line-truncate-length 0 nil "...") mode-line))
        (setq mode-line (replace-regexp-in-string "%n" (number-to-string (gethash 'track_number json)) mode-line))
        (setq mode-line (replace-regexp-in-string "%l" (format-seconds duration-format (/ (gethash 'duration json) 1000)) mode-line))
        (setq mode-line (replace-regexp-in-string "%s" (spotify-mode-line-shuffling-indicator (gethash 'player_shuffling json)) mode-line))
        (setq mode-line (replace-regexp-in-string "%r" (spotify-mode-line-repeating-indicator (gethash 'player_repeating json)) mode-line))
        (setq mode-line (replace-regexp-in-string "%p" (spotify-mode-line-playing-indicator (gethash 'player_state json)) mode-line))
        (spotify-update-mode-line mode-line)))))

(defun spotify-start-mode-line-timer ()
  "Starts the timer that updates the mode line according to the Spotify
   player status."
  (spotify-stop-mode-line-timer)
  (when (> spotify-mode-line-refresh-interval 0)
    (let ((first-run (format "%d sec" spotify-mode-line-refresh-interval))
          (interval spotify-mode-line-refresh-interval))
      (setq spotify-timer
            (run-at-time first-run interval 'spotify-refresh-mode-line)))))

(defun spotify-stop-mode-line-timer ()
  "Stops the timer that updates the mode line."
  (when (and (boundp 'spotify-timer) (timerp spotify-timer))
    (cancel-timer spotify-timer))
  (spotify-player-status))

(defun spotify-player-status ()
  "Updates the mode line to display the current Spotify player status."
  (interactive)
  (spotify-apply "player-status"))

(defun spotify-refresh-mode-line (&rest args)
  "Starts the player status process in order to update the mode line."
  (spotify-apply "player-status")
  (when spotify-mode-line-stale
    (setq spotify-mode-line-stale nil)
    (force-mode-line-update t)))

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

(defun spotify-is-repeating-supported()
  "Returns whether toggling repeat is supported."
  (interactive)
  (spotify-apply "is-repeating-supported"))

(defun spotify-is-shuffling-supported()
  "Returns whether toggling shuffle is supported."
  (interactive)
  (spotify-apply "is-shuffling-supported"))

(defun spotify-toggle-repeat ()
  "Sends a command to Spotify process to toggle the repeating flag."
  (interactive)
  (spotify-apply "toggle-repeat"))

(defun spotify-toggle-shuffle ()
  "Sends a command to Spotify process to toggle the shuffling flag."
  (interactive)
  (spotify-apply "toggle-shuffle"))

(defun spotify-is-repeating ()
  "Sends a command to Spotify process to get the current repeating state."
  (spotify-apply "is-repeating"))

(defun spotify-is-shuffling ()
  "Sends a command to the Spotify process to get the current shuffling state."
  (spotify-apply "is-shuffling"))

(provide 'spotify-controller)
