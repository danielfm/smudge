;;; spotify-client-controller.el --- Generic player controller interface for spotify-client  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Daniel Fernandes Martins

;;; Commentary:

;; This library defines a set of commands for controlling an instance of a Spotify client.  The
;; commands are sent via a multimethod-like dispatch to the chosen transport.

;;; Code:

(require 'spotify-client-api)

(defmacro spotify-client-if-gnu-linux (then else)
  "Evaluate THEN form if Emacs is running in GNU/Linux, otherwise evaluate ELSE form."
  `(if (eq system-type 'gnu/linux) ,then ,else))

(defmacro spotify-client-when-gnu-linux (then)
  "Evaluate THEN form if Emacs is running in GNU/Linux."
  `(spotify-client-if-gnu-linux ,then nil))

(defmacro spotify-client-if-darwin (then else)
  "Evaluate THEN form if Emacs is running in OS X, otherwise evaluate ELSE form."
  `(if (eq system-type 'darwin) ,then ,else))

(defmacro spotify-client-when-darwin (then)
  "Evaluate THEN form if Emacs is running in OS X."
  `(spotify-client-if-darwin ,then nil))

(defcustom spotify-client-transport 'connect
  "How the commands should be sent to Spotify process.
Defaults to 'connect, as it provides a consistent UX across all OSes."
  :type '(choice (symbol :tag "AppleScript" apple)
                 (symbol :tag "D-Bus" dbus)
                 (symbol :tag "Connect" connect))
  :group 'spotify-client)

(defcustom spotify-client-player-status-refresh-interval 5
  "The interval, in seconds, that the mode line must be updated.
When using the'connect transport, avoid using values smaller than 5
to avoid being rate limited.  Set to 0 to disable this feature."
  :type 'integer
  :group 'spotify-client)

(defcustom spotify-client-player-status-truncate-length 15
  "The maximum number of characters to truncate fields.
Fields will be truncated in `spotify-client-controller-player-status-format'."
  :type 'integer
  :group 'spotify-client)

(defcustom spotify-client-player-status-playing-text "Playing"
  "Text to be displayed when Spotify is playing."
  :type 'string
  :group 'spotify-client)

(defcustom spotify-client-player-status-paused-text "Paused"
  "Text to be displayed when Spotify is paused."
  :type 'string
  :group 'spotify-client)

(defcustom spotify-client-player-status-stopped-text "Stopped"
  "Text to be displayed when Spotify is stopped."
  :type 'string
  :group 'spotify-client)

(defcustom spotify-client-player-status-repeating-text "R"
  "Text to be displayed when repeat is enabled."
  :type 'string
  :group 'spotify-client)

(defcustom spotify-client-player-status-not-repeating-text "-"
  "Text to be displayed when repeat is disabled."
  :type 'string
  :group 'spotify-client)

(defcustom spotify-client-player-status-shuffling-text "S"
  "Text to be displayed when shuffling is enabled."
  :type 'string
  :group 'spotify-client)

(defcustom spotify-client-player-status-not-shuffling-text "-"
  "Text to be displayed when shuffling is disabled."
  :type 'string
  :group 'spotify-client)

(defcustom spotify-client-player-status-format "[%p: %a - %t ◷ %l %r%s]"
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
  :group 'spotify-client)

(defvar spotify-client-controller-timer nil)

(defvar spotify-client-controller-player-status ""
  "The text to be displayed in the global mode line or title bar.")

(defvar spotify-client-controller-player-metadata nil
  "The metadata about the currently playing track.")

(defun spotify-client-controller-apply (suffix &rest args)
  "Simple facility to emulate multimethods.
Apply SUFFIX to spotify-client-controller-prefixed functions, applying ARGS."
  (let ((func-name (format "spotify-client-%s-%s" spotify-client-transport suffix)))
    (apply (intern func-name) args)
    (unless (string= suffix "player-status")
      (run-at-time 1 nil 'spotify-client-controller-player-status))))

(defun spotify-client-controller-update-metadata (metadata)
  "Compose the playing status string to be displayed in the mode-line from METADATA."
  (let* ((player-status spotify-client-player-status-format)
         (duration-format "%m:%02s")
         (json-object-type 'hash-table)
         (json-key-type 'symbol)
         (json (condition-case nil
                   (json-read-from-string metadata)
                 (error (spotify-client-controller-update-player-status "")
                        nil))))
    (when json
      (progn
        (setq player-status (replace-regexp-in-string "%a" (truncate-string-to-width (gethash 'artist json) spotify-client-player-status-truncate-length 0 nil "...") player-status))
        (setq player-status (replace-regexp-in-string "%t" (truncate-string-to-width (gethash 'name json) spotify-client-player-status-truncate-length 0 nil "...") player-status))
        (setq player-status (replace-regexp-in-string "%n" (number-to-string (gethash 'track_number json)) player-status))
        (setq player-status (replace-regexp-in-string "%l" (format-seconds duration-format (/ (gethash 'duration json) 1000)) player-status))
        (setq player-status (replace-regexp-in-string "%s" (spotify-client-controller-player-status-shuffling-indicator (gethash 'player_shuffling json)) player-status))
        (setq player-status (replace-regexp-in-string "%r" (spotify-client-controller-player-status-repeating-indicator (gethash 'player_repeating json)) player-status))
        (setq player-status (replace-regexp-in-string "%p" (spotify-client-controller-player-status-playing-indicator (gethash 'player_state json)) player-status))
        (spotify-client-controller-update-player-status player-status)
        (setq spotify-client-controller-player-metadata json)))))

(defun spotify-client-controller-update-player-status (str)
  "Set the given STR to the player status, prefixed with the mode identifier."
  (when (not (string= str spotify-client-controller-player-status))
    (setq spotify-client-controller-player-status str)))

(defun spotify-client-controller-player-status-playing-indicator (str)
  "Return the value of the player state variable.
This value corresponding to the player's current state in STR."
  (cond ((string= "playing" str) spotify-client-player-status-playing-text)
        ((string= "stopped" str) spotify-client-player-status-stopped-text)
        ((string= "paused" str) spotify-client-player-status-paused-text)))

(defun spotify-client-controller-player-status-shuffling-indicator (shuffling)
  "Return the value of the shuffling state variable.
This value corresponds to the current SHUFFLING state."
  (if (eq shuffling t)
      spotify-client-player-status-shuffling-text
    spotify-client-player-status-not-shuffling-text))

(defun spotify-client-controller-player-status-repeating-indicator (repeating)
  "Return the value of the repeating state variable.
This corresponds to the current REPEATING state."
  (if (eq repeating t)
      spotify-client-player-status-repeating-text
    spotify-client-player-status-not-repeating-text))

(defun spotify-client-controller-timerp ()
	"Predicate to determine if the refresh timer is running."
  (and (boundp 'spotify-client-controller-timer) (timerp spotify-client-controller-timer)))

(defun spotify-client-controller-start-player-status-timer ()
 "Start the timer that will update the mode line according to the Spotify player status."
 (when (and (not (spotify-client-controller-timerp)) (> spotify-client-player-status-refresh-interval 0))
     (setq spotify-client-controller-timer
       (run-at-time t spotify-client-player-status-refresh-interval #'spotify-client-controller-player-status))))

(defun spotify-client-controller-stop-player-status-timer ()
 "Stop the timer that is updating the mode line."
 (when (spotify-client-controller-timerp)
   (cancel-timer spotify-client-controller-timer)))

(defun spotify-client-controller-player-status ()
  "Update the mode line to display the current Spotify player status."
  (interactive)
  (spotify-client-controller-apply "player-status"))

(defun spotify-client-controller-play-uri (uri)
  "Sends a `play' command to Spotify process passing the given URI."
  (interactive "SSpotify URI: ")
  (spotify-client-controller-apply "player-play-track" uri nil))

(defun spotify-client-controller-play-track (track &optional context)
  "Sends a `play' command to Spotify process with TRACK passing a CONTEXT id."
  (interactive)
  (spotify-client-controller-apply
   "player-play-track"
   (when track (spotify-client-api-get-item-uri track))
   (when context (spotify-client-api-get-item-uri context))))

(defun spotify-client-controller-toggle-play ()
  "Sends a `playpause' command to Spotify process."
  (interactive)
  (spotify-client-controller-apply "player-toggle-play"))

(defun spotify-client-controller-next-track ()
  "Sends a `next track' command to Spotify process."
  (interactive)
  (spotify-client-controller-apply "player-next-track"))

(defun spotify-client-controller-previous-track ()
  "Sends a `previous track' command to Spotify process."
  (interactive)
  (spotify-client-controller-apply "player-previous-track"))

(defun spotify-client-controller-volume-up ()
  "Increase the volume for the active device."
  (interactive)
  (spotify-client-controller-apply "volume-up"))

(defun spotify-client-controller-volume-down ()
  "Increase the volume for the active device."
  (interactive)
  (spotify-client-controller-apply "volume-down"))

(defun spotify-client-controller-volume-mute-unmute ()
  "Mute/unmute the volume for the active device."
  (interactive)
  (spotify-client-controller-apply "volume-mute-unmute"))

(defun spotify-client-controller-toggle-repeat ()
  "Sends a command to Spotify process to toggle the repeating flag."
  (interactive)
  (spotify-client-controller-apply "toggle-repeat"))

(defun spotify-client-controller-toggle-shuffle ()
  "Sends a command to Spotify process to toggle the shuffling flag."
  (interactive)
  (spotify-client-controller-apply "toggle-shuffle"))

(defun spotify-client-controller-is-repeating ()
  "Sends a command to Spotify process to get the current repeating state."
  (spotify-client-controller-apply "is-repeating"))

(defun spotify-client-controller-is-shuffling ()
  "Sends a command to the Spotify process to get the current shuffling state."
  (spotify-client-controller-apply "is-shuffling"))

(provide 'spotify-client-controller)
;;; spotify-client-controller.el ends here
