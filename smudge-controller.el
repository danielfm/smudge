;;; smudge-controller.el --- Generic player controller interface for Smudge  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Daniel Fernandes Martins

;; SPDX-License-Identifier:  GPL-3.0-or-later

;;; Commentary:

;; This library defines a set of commands for controlling an instance of a Spotify client.  The
;; commands are sent via a multimethod-like dispatch to the chosen transport.

;;; Code:

(require 'smudge-api)

(defmacro smudge-if-gnu-linux (then else)
  "Evaluate THEN form if Emacs is running in GNU/Linux, otherwise evaluate ELSE form."
  `(if (eq system-type 'gnu/linux) ,then ,else))

(defmacro smudge-when-gnu-linux (then)
  "Evaluate THEN form if Emacs is running in GNU/Linux."
  `(smudge-if-gnu-linux ,then nil))

(defmacro smudge-if-darwin (then else)
  "Evaluate THEN form if Emacs is running in OS X, otherwise evaluate ELSE form."
  `(if (eq system-type 'darwin) ,then ,else))

(defmacro smudge-when-darwin (then)
  "Evaluate THEN form if Emacs is running in OS X."
  `(smudge-if-darwin ,then nil))

(defcustom smudge-transport 'connect
  "How the commands should be sent to Spotify process.
Defaults to 'connect, as it provides a consistent UX across all OSes."
  :type '(choice (symbol :tag "AppleScript" apple)
                 (symbol :tag "D-Bus" dbus)
                 (symbol :tag "Connect" connect))
  :group 'smudge)

(defcustom smudge-player-status-refresh-interval 5
  "The interval, in seconds, that the mode line must be updated.
When using the'connect transport, avoid using values smaller than 5
to avoid being rate limited.  Set to 0 to disable this feature."
  :type 'integer
  :group 'smudge)

(defcustom smudge-player-status-truncate-length 15
  "The maximum number of characters to truncate fields.
Fields will be truncated in `smudge-controller-player-status-format'."
  :type 'integer
  :group 'smudge)

(defcustom smudge-player-status-playing-text "Playing"
  "Text to be displayed when Spotify is playing."
  :type 'string
  :group 'smudge)

(defcustom smudge-player-status-paused-text "Paused"
  "Text to be displayed when Spotify is paused."
  :type 'string
  :group 'smudge)

(defcustom smudge-player-status-stopped-text "Stopped"
  "Text to be displayed when Spotify is stopped."
  :type 'string
  :group 'smudge)

(defcustom smudge-player-status-repeating-text "R"
  "Text to be displayed when repeat is enabled."
  :type 'string
  :group 'smudge)

(defcustom smudge-player-status-not-repeating-text "-"
  "Text to be displayed when repeat is disabled."
  :type 'string
  :group 'smudge)

(defcustom smudge-player-status-shuffling-text "S"
  "Text to be displayed when shuffling is enabled."
  :type 'string
  :group 'smudge)

(defcustom smudge-player-status-not-shuffling-text "-"
  "Text to be displayed when shuffling is disabled."
  :type 'string
  :group 'smudge)

(defcustom smudge-player-status-format "[%p: %a - %t ◷ %l %r%s]"
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
  :group 'smudge)

(defvar smudge-controller-timer nil)

(defvar smudge-controller-player-status ""
  "The text to be displayed in the global mode line or title bar.")

(defvar smudge-controller-player-metadata nil
  "The metadata about the currently playing track.")

(defun smudge-controller-apply (suffix &rest args)
  "Simple facility to emulate multimethods.
Apply SUFFIX to smudge-controller-prefixed functions, applying ARGS."
  (let ((func-name (format "smudge-%s-%s" smudge-transport suffix)))
    (apply (intern func-name) args)
    (unless (string= suffix "player-status")
      (run-at-time 1 nil #'smudge-controller-player-status))))

(defun smudge-controller-update-metadata (metadata)
  "Compose the playing status string to be displayed in the mode-line from METADATA."
  (let* ((player-status smudge-player-status-format)
         (duration-format "%m:%02s")
         (json-object-type 'hash-table)
         (json-key-type 'symbol)
         (json (condition-case nil
                   (json-read-from-string metadata)
                 (error (smudge-controller-update-player-status "")
                        nil))))
    (when json
      (progn
        (setq player-status (replace-regexp-in-string "%a" (truncate-string-to-width (gethash 'artist json) smudge-player-status-truncate-length 0 nil "...") player-status))
        (setq player-status (replace-regexp-in-string "%t" (truncate-string-to-width (gethash 'name json) smudge-player-status-truncate-length 0 nil "...") player-status))
        (setq player-status (replace-regexp-in-string "%n" (number-to-string (gethash 'track_number json)) player-status))
        (setq player-status (replace-regexp-in-string "%l" (format-seconds duration-format (/ (gethash 'duration json) 1000)) player-status))
        (setq player-status (replace-regexp-in-string "%s" (smudge-controller-player-status-shuffling-indicator (gethash 'player_shuffling json)) player-status))
        (setq player-status (replace-regexp-in-string "%r" (smudge-controller-player-status-repeating-indicator (gethash 'player_repeating json)) player-status))
        (setq player-status (replace-regexp-in-string "%p" (smudge-controller-player-status-playing-indicator (gethash 'player_state json)) player-status))
        (smudge-controller-update-player-status player-status)
        (setq smudge-controller-player-metadata json)))))

(defun smudge-controller-update-player-status (str)
  "Set the given STR to the player status, prefixed with the mode identifier."
  (unless (string= str smudge-controller-player-status)
    (setq smudge-controller-player-status str)))

(defun smudge-controller-player-status-playing-indicator (str)
  "Return the value of the player state variable.
This value corresponding to the player's current state in STR."
  (cond ((string= "playing" str) smudge-player-status-playing-text)
        ((string= "stopped" str) smudge-player-status-stopped-text)
        ((string= "paused" str) smudge-player-status-paused-text)))

(defun smudge-controller-player-status-shuffling-indicator (shuffling)
  "Return the value of the shuffling state variable.
This value corresponds to the current SHUFFLING state."
  (if (eq shuffling t)
      smudge-player-status-shuffling-text
    smudge-player-status-not-shuffling-text))

(defun smudge-controller-player-status-repeating-indicator (repeating)
  "Return the value of the repeating state variable.
This corresponds to the current REPEATING state."
  (if (eq repeating t)
      smudge-player-status-repeating-text
    smudge-player-status-not-repeating-text))

(defun smudge-controller-timerp ()
  "Predicate to determine if the refresh timer is running."
  (and (boundp 'smudge-controller-timer) (timerp smudge-controller-timer)))

(defun smudge-controller-start-player-status-timer ()
  "Start the timer that will update the mode line according to the Spotify player status."
  (when (and (not (smudge-controller-timerp)) (> smudge-player-status-refresh-interval 0))
    (setq smudge-controller-timer
          (run-at-time t smudge-player-status-refresh-interval #'smudge-controller-player-status))))

(defun smudge-controller-stop-player-status-timer ()
  "Stop the timer that is updating the mode line."
  (when (smudge-controller-timerp)
    (cancel-timer smudge-controller-timer)))

(defun smudge-controller-player-status ()
  "Update the mode line to display the current Spotify player status."
  (interactive)
  (smudge-controller-apply "player-status"))

(defun smudge-controller-play-uri (uri)
  "Sends a `play' command to Spotify process passing the given URI."
  (interactive "SSpotify URI: ")
  (smudge-controller-apply "player-play-track" uri nil))

(defun smudge-controller-play-track (track &optional context)
  "Sends a `play' command to Spotify process with TRACK passing a CONTEXT id."
  (interactive)
  (smudge-controller-apply
   "player-play-track"
   (when track (smudge-api-get-item-uri track))
   (when context (smudge-api-get-item-uri context))))

(defun smudge-controller-toggle-play ()
  "Sends a `playpause' command to Spotify process."
  (interactive)
  (smudge-controller-apply "player-toggle-play"))

(defun smudge-controller-next-track ()
  "Sends a `next track' command to Spotify process."
  (interactive)
  (smudge-controller-apply "player-next-track"))

(defun smudge-controller-previous-track ()
  "Sends a `previous track' command to Spotify process."
  (interactive)
  (smudge-controller-apply "player-previous-track"))

(defun smudge-controller-volume-up ()
  "Increase the volume for the active device."
  (interactive)
  (smudge-controller-apply "volume-up"))

(defun smudge-controller-volume-down ()
  "Increase the volume for the active device."
  (interactive)
  (smudge-controller-apply "volume-down"))

(defun smudge-controller-volume-mute-unmute ()
  "Mute/unmute the volume for the active device."
  (interactive)
  (smudge-controller-apply "volume-mute-unmute"))

(defun smudge-controller-toggle-repeat ()
  "Sends a command to Spotify process to toggle the repeating flag."
  (interactive)
  (smudge-controller-apply "toggle-repeat"))

(defun smudge-controller-toggle-shuffle ()
  "Sends a command to Spotify process to toggle the shuffling flag."
  (interactive)
  (smudge-controller-apply "toggle-shuffle"))

(defun smudge-controller-is-repeating ()
  "Sends a command to Spotify process to get the current repeating state."
  (smudge-controller-apply "is-repeating"))

(defun smudge-controller-is-shuffling ()
  "Sends a command to the Spotify process to get the current shuffling state."
  (smudge-controller-apply "is-shuffling"))

(provide 'smudge-controller)
;;; smudge-controller.el ends here
