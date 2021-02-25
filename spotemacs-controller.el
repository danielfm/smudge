;;; spotemacs-controller.el --- Generic player controller interface for Spotemacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Daniel Fernandes Martins

;;; Commentary:

;; This library defines a set of commands for controlling an instance of a Spotify client.  The
;; commands are sent via a multimethod-like dispatch to the chosen transport.

;;; Code:

(require 'spotemacs-api)

(defmacro spotemacs-if-gnu-linux (then else)
  "Evaluate THEN form if Emacs is running in GNU/Linux, otherwise evaluate ELSE form."
  `(if (eq system-type 'gnu/linux) ,then ,else))

(defmacro spotemacs-when-gnu-linux (then)
  "Evaluate THEN form if Emacs is running in GNU/Linux."
  `(spotemacs-if-gnu-linux ,then nil))

(defmacro spotemacs-if-darwin (then else)
  "Evaluate THEN form if Emacs is running in OS X, otherwise evaluate ELSE form."
  `(if (eq system-type 'darwin) ,then ,else))

(defmacro spotemacs-when-darwin (then)
  "Evaluate THEN form if Emacs is running in OS X."
  `(spotemacs-if-darwin ,then nil))

(defcustom spotemacs-transport 'connect
  "How the commands should be sent to Spotify process.
Defaults to 'connect, as it provides a consistent UX across all OSes."
  :type '(choice (symbol :tag "AppleScript" apple)
                 (symbol :tag "D-Bus" dbus)
                 (symbol :tag "Connect" connect))
  :group 'spotemacs)

(defcustom spotemacs-player-status-refresh-interval 5
  "The interval, in seconds, that the mode line must be updated.
When using the'connect transport, avoid using values smaller than 5
to avoid being rate limited.  Set to 0 to disable this feature."
  :type 'integer
  :group 'spotemacs)

(defcustom spotemacs-player-status-truncate-length 15
  "The maximum number of characters to truncate fields.
Fields will be truncated in `spotemacs-controller-player-status-format'."
  :type 'integer
  :group 'spotemacs)

(defcustom spotemacs-player-status-playing-text "Playing"
  "Text to be displayed when Spotify is playing."
  :type 'string
  :group 'spotemacs)

(defcustom spotemacs-player-status-paused-text "Paused"
  "Text to be displayed when Spotify is paused."
  :type 'string
  :group 'spotemacs)

(defcustom spotemacs-player-status-stopped-text "Stopped"
  "Text to be displayed when Spotify is stopped."
  :type 'string
  :group 'spotemacs)

(defcustom spotemacs-player-status-repeating-text "R"
  "Text to be displayed when repeat is enabled."
  :type 'string
  :group 'spotemacs)

(defcustom spotemacs-player-status-not-repeating-text "-"
  "Text to be displayed when repeat is disabled."
  :type 'string
  :group 'spotemacs)

(defcustom spotemacs-player-status-shuffling-text "S"
  "Text to be displayed when shuffling is enabled."
  :type 'string
  :group 'spotemacs)

(defcustom spotemacs-player-status-not-shuffling-text "-"
  "Text to be displayed when shuffling is disabled."
  :type 'string
  :group 'spotemacs)

(defcustom spotemacs-player-status-format "[%p: %a - %t ◷ %l %r%s]"
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
  :group 'spotemacs)

(defvar spotemacs-controller-timer nil)

(defvar spotemacs-controller-player-status ""
  "The text to be displayed in the global mode line or title bar.")

(defvar spotemacs-controller-player-metadata nil
  "The metadata about the currently playing track.")

(defun spotemacs-controller-apply (suffix &rest args)
  "Simple facility to emulate multimethods.
Apply SUFFIX to spotemacs-controller-prefixed functions, applying ARGS."
  (let ((func-name (format "spotemacs-%s-%s" spotemacs-transport suffix)))
    (apply (intern func-name) args)
    (unless (string= suffix "player-status")
      (run-at-time 1 nil 'spotemacs-controller-player-status))))

(defun spotemacs-controller-update-metadata (metadata)
  "Compose the playing status string to be displayed in the mode-line from METADATA."
  (let* ((player-status spotemacs-player-status-format)
         (duration-format "%m:%02s")
         (json-object-type 'hash-table)
         (json-key-type 'symbol)
         (json (condition-case nil
                   (json-read-from-string metadata)
                 (error (spotemacs-controller-update-player-status "")
                        nil))))
    (when json
      (progn
        (setq player-status (replace-regexp-in-string "%a" (truncate-string-to-width (gethash 'artist json) spotemacs-player-status-truncate-length 0 nil "...") player-status))
        (setq player-status (replace-regexp-in-string "%t" (truncate-string-to-width (gethash 'name json) spotemacs-player-status-truncate-length 0 nil "...") player-status))
        (setq player-status (replace-regexp-in-string "%n" (number-to-string (gethash 'track_number json)) player-status))
        (setq player-status (replace-regexp-in-string "%l" (format-seconds duration-format (/ (gethash 'duration json) 1000)) player-status))
        (setq player-status (replace-regexp-in-string "%s" (spotemacs-controller-player-status-shuffling-indicator (gethash 'player_shuffling json)) player-status))
        (setq player-status (replace-regexp-in-string "%r" (spotemacs-controller-player-status-repeating-indicator (gethash 'player_repeating json)) player-status))
        (setq player-status (replace-regexp-in-string "%p" (spotemacs-controller-player-status-playing-indicator (gethash 'player_state json)) player-status))
        (spotemacs-controller-update-player-status player-status)
        (setq spotemacs-controller-player-metadata json)))))

(defun spotemacs-controller-update-player-status (str)
  "Set the given STR to the player status, prefixed with the mode identifier."
  (when (not (string= str spotemacs-controller-player-status))
    (setq spotemacs-controller-player-status str)))

(defun spotemacs-controller-player-status-playing-indicator (str)
  "Return the value of the player state variable.
This value corresponding to the player's current state in STR."
  (cond ((string= "playing" str) spotemacs-player-status-playing-text)
        ((string= "stopped" str) spotemacs-player-status-stopped-text)
        ((string= "paused" str) spotemacs-player-status-paused-text)))

(defun spotemacs-controller-player-status-shuffling-indicator (shuffling)
  "Return the value of the shuffling state variable.
This value corresponds to the current SHUFFLING state."
  (if (eq shuffling t)
      spotemacs-player-status-shuffling-text
    spotemacs-player-status-not-shuffling-text))

(defun spotemacs-controller-player-status-repeating-indicator (repeating)
  "Return the value of the repeating state variable.
This corresponds to the current REPEATING state."
  (if (eq repeating t)
      spotemacs-player-status-repeating-text
    spotemacs-player-status-not-repeating-text))

(defun spotemacs-controller-timerp ()
	"Predicate to determine if the refresh timer is running."
  (and (boundp 'spotemacs-controller-timer) (timerp spotemacs-controller-timer)))

(defun spotemacs-controller-start-player-status-timer ()
 "Start the timer that will update the mode line according to the Spotify player status."
 (when (and (not (spotemacs-controller-timerp)) (> spotemacs-player-status-refresh-interval 0))
     (setq spotemacs-controller-timer
       (run-at-time t spotemacs-player-status-refresh-interval #'spotemacs-controller-player-status))))

(defun spotemacs-controller-stop-player-status-timer ()
 "Stop the timer that is updating the mode line."
 (when (spotemacs-controller-timerp)
   (cancel-timer spotemacs-controller-timer)))

(defun spotemacs-controller-player-status ()
  "Update the mode line to display the current Spotify player status."
  (interactive)
  (spotemacs-controller-apply "player-status"))

(defun spotemacs-controller-play-uri (uri)
  "Sends a `play' command to Spotify process passing the given URI."
  (interactive "SSpotify URI: ")
  (spotemacs-controller-apply "player-play-track" uri nil))

(defun spotemacs-controller-play-track (track &optional context)
  "Sends a `play' command to Spotify process with TRACK passing a CONTEXT id."
  (interactive)
  (spotemacs-controller-apply
   "player-play-track"
   (when track (spotemacs-api-get-item-uri track))
   (when context (spotemacs-api-get-item-uri context))))

(defun spotemacs-controller-toggle-play ()
  "Sends a `playpause' command to Spotify process."
  (interactive)
  (spotemacs-controller-apply "player-toggle-play"))

(defun spotemacs-controller-next-track ()
  "Sends a `next track' command to Spotify process."
  (interactive)
  (spotemacs-controller-apply "player-next-track"))

(defun spotemacs-controller-previous-track ()
  "Sends a `previous track' command to Spotify process."
  (interactive)
  (spotemacs-controller-apply "player-previous-track"))

(defun spotemacs-controller-volume-up ()
  "Increase the volume for the active device."
  (interactive)
  (spotemacs-controller-apply "volume-up"))

(defun spotemacs-controller-volume-down ()
  "Increase the volume for the active device."
  (interactive)
  (spotemacs-controller-apply "volume-down"))

(defun spotemacs-controller-volume-mute-unmute ()
  "Mute/unmute the volume for the active device."
  (interactive)
  (spotemacs-controller-apply "volume-mute-unmute"))

(defun spotemacs-controller-toggle-repeat ()
  "Sends a command to Spotify process to toggle the repeating flag."
  (interactive)
  (spotemacs-controller-apply "toggle-repeat"))

(defun spotemacs-controller-toggle-shuffle ()
  "Sends a command to Spotify process to toggle the shuffling flag."
  (interactive)
  (spotemacs-controller-apply "toggle-shuffle"))

(defun spotemacs-controller-is-repeating ()
  "Sends a command to Spotify process to get the current repeating state."
  (spotemacs-controller-apply "is-repeating"))

(defun spotemacs-controller-is-shuffling ()
  "Sends a command to the Spotify process to get the current shuffling state."
  (spotemacs-controller-apply "is-shuffling"))

(provide 'spotemacs-controller)
;;; spotemacs-controller.el ends here
