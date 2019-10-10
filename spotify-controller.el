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

(defcustom spotify-player-status-refresh-interval 5
  "The interval, in seconds, that the mode line must be updated. When using the
'connect transport, avoid using values smaller than 5 to avoid being rate
limited. Set to 0 to disable this feature."
  :type 'integer
  :group 'spotify)

(defcustom spotify-player-status-truncate-length 15
  "The maximum number of characters to truncated fields in `spotify-player-status-format'."
  :type 'integer
  :group 'spotify)

(defcustom spotify-player-status-playing-text "Playing"
  "Text to be displayed when Spotify is playing."
  :type 'string
  :group 'spotify)

(defcustom spotify-player-status-paused-text "Paused"
  "Text to be displayed when Spotify is paused."
  :type 'string
  :group 'spotify)

(defcustom spotify-player-status-stopped-text "Stopped"
  "Text to be displayed when Spotify is stopped."
  :type 'string
  :group 'spotify)

(defcustom spotify-player-status-repeating-text "R"
  "Text to be displayed when repeat is enabled."
  :type 'string
  :group 'spotify)

(defcustom spotify-player-status-not-repeating-text "-"
  "Text to be displayed when repeat is disabled."
  :type 'string
  :group 'spotify)

(defcustom spotify-player-status-shuffling-text "S"
  "Text to be displayed when shuffling is enabled."
  :type 'string
  :group 'spotify)

(defcustom spotify-player-status-not-shuffling-text "-"
  "Text to be displayed when shuffling is disabled."
  :type 'string
  :group 'spotify)

(defcustom spotify-player-status-format "[%p: %a - %t ◷ %l %r%s]"
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

(defun spotify-player-status-playing-indicator (str)
  "Return the value of the player state variable.
This value corresponding to the player's current state in STR."
  (cond ((string= "playing" str) spotify-player-status-playing-text)
        ((string= "stopped" str) spotify-player-status-stopped-text)
        ((string= "paused" str) spotify-player-status-paused-text)))

(defun spotify-player-status-shuffling-indicator (shuffling)
  "Return the value of the shuffling state variable.
This value corresponds to the current SHUFFLING state."
  (if (eq shuffling t)
      spotify-player-status-shuffling-text
    spotify-player-status-not-shuffling-text))

(defun spotify-player-status-repeating-indicator (repeating)
  "Return the value of the repeating state variable.
This corresponds to the current REPEATING state."
  (if (eq repeating t)
      spotify-player-status-repeating-text
    spotify-player-status-not-repeating-text))

(defun spotify-replace-player-status-flags (metadata)
  "Compose the playing status string to be displayed in the player-status from METADATA."
  (let* ((player-status spotify-player-status-format)
         (duration-format "%m:%02s")
         (json-object-type 'hash-table)
         (json-key-type 'symbol)
         (json (condition-case nil
                   (json-read-from-string metadata)
                 (error (spotify-update-player-status "")
                        nil))))
    (when json
      (progn
        (setq player-status (replace-regexp-in-string "%a" (truncate-string-to-width (gethash 'artist json) spotify-player-status-truncate-length 0 nil "...") player-status))
        (setq player-status (replace-regexp-in-string "%t" (truncate-string-to-width (gethash 'name json) spotify-player-status-truncate-length 0 nil "...") player-status))
        (setq player-status (replace-regexp-in-string "%n" (number-to-string (gethash 'track_number json)) player-status))
        (setq player-status (replace-regexp-in-string "%l" (format-seconds duration-format (/ (gethash 'duration json) 1000)) player-status))
        (setq player-status (replace-regexp-in-string "%s" (spotify-player-status-shuffling-indicator (gethash 'player_shuffling json)) player-status))
        (setq player-status (replace-regexp-in-string "%r" (spotify-player-status-repeating-indicator (gethash 'player_repeating json)) player-status))
        (setq player-status (replace-regexp-in-string "%p" (spotify-player-status-playing-indicator (gethash 'player_state json)) player-status))
        (spotify-update-player-status player-status)))))

(defun spotify-start-player-status-timer ()
  "Start the timer that will update the mode line according to the Spotify player status."
  (spotify-stop-player-status-timer)
  (when (> spotify-player-status-refresh-interval 0)
    (let ((first-run (format "%d sec" spotify-player-status-refresh-interval))
          (interval spotify-player-status-refresh-interval))
      (setq spotify-timer
            (run-at-time first-run interval 'spotify-player-status)))))

(defun spotify-stop-player-status-timer ()
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
