;;; package --- Summary

;;; Commentary:

;;; spotify-controller.el --- Generic player controller interface for Spotify.el

;; Copyright (C) 2014-2019 Daniel Fernandes Martins

;;; Code:

(require 'spotify-remote)

(defmacro if-gnu-linux (then else)
  "Evaluate THEN form if Emacs is running in GNU/Linux, otherwise evaluate ELSE form."
  `(if (eq system-type 'gnu/linux) ,then ,else))

(defmacro when-gnu-linux (then)
  "Evaluate THEN form if Emacs is running in GNU/Linux."
  `(if-gnu-linux ,then nil))

(defmacro if-darwin (then else)
  "Evaluate THEN form if Emacs is running in OS X, otherwise evaluate ELSE form."
  `(if (eq system-type 'darwin) ,then ,else))

(defmacro when-darwin (then)
  "Evaluate THEN form if Emacs is running in OS X."
  `(if-darwin ,then nil))

(defcustom spotify-transport 'connect
  "How the commands should be sent to Spotify process. Defaults to 'connect, as it provides a consistent UX across all OSes."
  :type '(choice (symbol :tag "AppleScript" apple)
                 (symbol :tag "D-Bus" dbus)
                 (symbol :tag "Connect" connect))
  :group 'spotify)

(defcustom spotify-mode-line-refresh-interval 5
  "The interval, in seconds, that the mode line must be updated. When using the
'connect transport, avoid using values smaller than 5 to avoid being rate
limited. Set to 0 to disable this feature."
  :type 'integer
  :group 'spotify)

(defcustom spotify-mode-line-truncate-length 15
  "The maximum number of characters to truncated fields in `spotify-mode-line-format'."
  :type 'integer
  :group 'spotify)

(defcustom spotify-mode-line-playing-text "Playing"
  "Text to be displayed when Spotify is playing."
  :type 'string
  :group 'spotify)

(defcustom spotify-mode-line-paused-text "Paused"
  "Text to be displayed when Spotify is paused."
  :type 'string
  :group 'spotify)

(defcustom spotify-mode-line-stopped-text "Stopped"
  "Text to be displayed when Spotify is stopped."
  :type 'string
  :group 'spotify)

(defcustom spotify-mode-line-repeating-text "R"
  "Text to be displayed when repeat is enabled."
  :type 'string
  :group 'spotify)

(defcustom spotify-mode-line-not-repeating-text "-"
  "Text to be displayed when repeat is disabled."
  :type 'string
  :group 'spotify)

(defcustom spotify-mode-line-shuffling-text "S"
  "Text to be displayed when shuffling is enabled."
  :type 'string
  :group 'spotify)

(defcustom spotify-mode-line-not-shuffling-text "-"
  "Text to be displayed when shuffling is disabled."
  :type 'string
  :group 'spotify)

(defcustom spotify-mode-line-format "[%p: %a - %t ◷ %l %r%s]"
  "Format used to display the current Spotify client player status.
The following placeholders are supported:

* %a - Artist name (truncated)
* %t - Track name (truncated)
* %n - Track #
* %l - Track duration, in minutes (i.e. 01:35)
* %p - Player status indicator for 'playing', 'paused', and 'stopped' states
* %s - Player shuffling status indicator
* %r - Player repeating status indicator"
  :type 'string
  :group 'spotify)

(defvar spotify-timer nil)

(defun spotify-apply (suffix &rest args)
  "Simple facility to emulate multimethods.
Apply SUFFIX to spotify-prefixed functions, applying ARGS."
  (let ((func-name (format "spotify-%s-%s" spotify-transport suffix)))
    (apply (intern func-name) args)))

(defun spotify-mode-line-playing-indicator (str)
  "Return the value of the player state variable.
This value corresponding to the player's current state in STR."
  (cond ((string= "playing" str) spotify-mode-line-playing-text)
        ((string= "stopped" str) spotify-mode-line-stopped-text)
        ((string= "paused" str) spotify-mode-line-paused-text)))

(defun spotify-mode-line-shuffling-indicator (shuffling)
  "Return the value of the shuffling state variable.
This value corresponds to the current SHUFFLING state."
  (if (eq shuffling t)
      spotify-mode-line-shuffling-text
    spotify-mode-line-not-shuffling-text))

(defun spotify-mode-line-repeating-indicator (repeating)
  "Return the value of the repeating state variable.
This corresponds to the current REPEATING state."
  (if (eq repeating t)
      spotify-mode-line-repeating-text
    spotify-mode-line-not-repeating-text))

(defun spotify-replace-mode-line-flags (metadata)
  "Compose the playing status string to be displayed in the mode-line from METADATA."
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
  "Start the timer that will update the mode line according to the Spotify player status."
  (spotify-stop-mode-line-timer)
  (when (> spotify-mode-line-refresh-interval 0)
    (let ((first-run (format "%d sec" spotify-mode-line-refresh-interval))
          (interval spotify-mode-line-refresh-interval))
      (setq spotify-timer
            (run-at-time first-run interval 'spotify-player-status)))))

(defun spotify-stop-mode-line-timer ()
  "Stop the timer that is updating the mode line."
  (when (and (boundp 'spotify-timer) (timerp spotify-timer))
    (cancel-timer spotify-timer))
  (spotify-player-status))

(defun spotify-player-status ()
  "Update the mode line to display the current Spotify player status."
  (interactive)
  (spotify-apply "player-status")
  (force-mode-line-update t))

(defun spotify-play-uri (uri)
  "Sends a `play' command to Spotify process passing the given URI."
  (interactive "SSpotify URI: ")
  (spotify-apply "player-play-track" uri nil))

(defun spotify-play-track (track &optional context)
  "Sends a `play' command to Spotify process with TRACK passing a CONTEXT id."
  (interactive)
  (spotify-apply
   "player-play-track"
   (when track (spotify-get-item-uri track))
   (when context (spotify-get-item-uri context))))

(defun spotify-toggle-play ()
  "Sends a `playpause' command to Spotify process."
  (interactive)
  (spotify-apply "player-toggle-play"))

(defun spotify-next-track ()
  "Sends a `next track' command to Spotify process."
  (interactive)
  (spotify-apply "player-next-track"))

(defun spotify-previous-track ()
  "Sends a `previous track' command to Spotify process."
  (interactive)
  (spotify-apply "player-previous-track"))

(defun spotify-volume-up ()
  "Increase the volume for the active device."
  (interactive)
  (spotify-apply "volume-up"))

(defun spotify-volume-down ()
  "Increase the volume for the active device."
  (interactive)
  (spotify-apply "volume-down"))

(defun spotify-volume-mute-unmute ()
  "Mute/unmute the volume for the active device."
  (interactive)
  (spotify-apply "volume-mute-unmute"))

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

;;; spotify-controller.el ends here
