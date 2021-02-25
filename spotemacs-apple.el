;;; spotemacs-apple.el --- Apple-specific code for Spotemacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Daniel Fernandes Martins

;;; Commentary:

;; This library handles controlling Spotify via Applescript commands.  It implements a set of
;; multimethod-like functions that are dispatched in spotemacs-controller.el.

;;; Code:

(require 'spotemacs-controller)

(defvar spotemacs-apple-player-status-script)
(defvar spotemacs-apple-player-status-script-file)

(defcustom spotemacs-osascript-bin-path "/usr/bin/osascript"
  "Path to `osascript' binary."
  :group 'spotemacs
  :type 'string)

; Do not change this unless you know what you're doing
(setq spotemacs-apple-player-status-script "
# Source: https://github.com/andrehaveman/spotemacs-node-applescript
on escape_quotes(string_to_escape)
  set AppleScript's text item delimiters to the \"\\\"\"
  set the item_list to every text item of string_to_escape
  set AppleScript's text item delimiters to the \"\\\\\\\"\"
  set string_to_escape to the item_list as string
  set AppleScript's text item delimiters to \"\"
  return string_to_escape
end escape_quotes

tell application \"Spotify\"
  if it is running then
    set ctrack to \"{\"
    set ctrack to ctrack & \"\\\"artist\\\": \\\"\" & my escape_quotes(current track's artist) & \"\\\"\"
    set ctrack to ctrack & \",\\\"duration\\\": \" & current track's duration
    set ctrack to ctrack & \",\\\"track_number\\\": \" & current track's track number
    set ctrack to ctrack & \",\\\"name\\\": \\\"\" & my escape_quotes(current track's name) & \"\\\"\"
    set ctrack to ctrack & \",\\\"player_state\\\": \\\"\" & player state & \"\\\"\"
    set ctrack to ctrack & \",\\\"player_shuffling\\\": \" & shuffling
    set ctrack to ctrack & \",\\\"player_repeating\\\": \" & repeating
    set ctrack to ctrack & \"}\"
  end if
end tell
")

;; Write script to a temp file
(setq spotemacs-apple-player-status-script-file
      (make-temp-file "spotemacs.el" nil nil spotemacs-apple-player-status-script))

(defun spotemacs-apple-command-line (cmd)
  "Return a command line prefix for any Spotify command CMD."
  (format "%s -e %s"
          spotemacs-osascript-bin-path
          (shell-quote-argument (format "tell application \"Spotify\" to %s" cmd))))

(defun spotemacs-apple-command (cmd)
  "Send the given CMD to the Spotify client.
Return the resulting status string."
  (replace-regexp-in-string
   "\n$" ""
   (shell-command-to-string (spotemacs-apple-command-line cmd))))

(defun spotemacs-apple-set-player-status-from-process-output (process output)
  "Set the OUTPUT of the player status PROCESS to the player status."
  (spotemacs-controller-update-metadata output)
  (with-current-buffer (process-buffer process)
    (delete-region (point-min) (point-max))))

(defun spotemacs-apple-player-status ()
  "Update the player status to display the current Spotify player status."
  (let* ((process-name "spotemacs-player-status")
         (process-status (process-status process-name))
         (cmd (format "%s %s" spotemacs-osascript-bin-path spotemacs-apple-player-status-script-file)))
    (when (not process-status)
      (let* ((default-directory user-emacs-directory)
             (process (start-process-shell-command process-name "*spotemacs-player-status*" cmd)))
        (set-process-filter process 'spotemacs-apple-set-player-status-from-process-output)))))

(defun spotemacs-apple-player-state ()
  "Dispatch get player state."
  (spotemacs-apple-command "get player state"))

(defun spotemacs-apple-player-toggle-play ()
  "Dispatch playpause."
  (spotemacs-apple-command "playpause"))

(defun spotemacs-apple-player-next-track ()
  "Dispatch next track."
  (spotemacs-apple-command "next track"))

(defun spotemacs-apple-player-previous-track ()
  "Dispatch previous track."
  (spotemacs-apple-command "previous track"))

(defun spotemacs-apple-volume-up ()
  "Send message about inability to change volume."
  (message "Changing the volume not supported by the Spotify AppleScript client"))

(defun spotemacs-apple-volume-down ()
  "Send message about inability to change volume."
  (message "Changing the volume not supported by the Spotify AppleScript client"))

(defun spotemacs-apple-volume-mute-unmute ()
  "Send message about inability to change volume."
  (message "Changing the volume not supported by the Spotify AppleScript client"))

(defun spotemacs-apple-toggle-repeat ()
  "Dispatch repeat command."
  (spotemacs-apple-command "set repeating to not repeating"))

(defun spotemacs-apple-toggle-shuffle ()
  "Dispatch shuffle command."
  (spotemacs-apple-command "set shuffling to not shuffling"))

(defun spotemacs-apple-player-play-track (track-id context-id)
  "Dispatch message about playing TRACK-ID in CONTEXT-ID."
  (spotemacs-apple-command (format "play track \"%s\" in context \"%s\"" track-id context-id)))

(provide 'spotemacs-apple)
;;; spotemacs-apple.el ends here
