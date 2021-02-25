;;; spotify-client-apple.el --- Apple-specific code for spotify-client  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Daniel Fernandes Martins

;;; Commentary:

;; This library handles controlling Spotify via Applescript commands.  It implements a set of
;; multimethod-like functions that are dispatched in spotify-client-controller.el.

;;; Code:

(require 'spotify-client-controller)

(defvar spotify-client-apple-player-status-script)
(defvar spotify-client-apple-player-status-script-file)

(defcustom spotify-client-osascript-bin-path "/usr/bin/osascript"
  "Path to `osascript' binary."
  :group 'spotify-client
  :type 'string)

; Do not change this unless you know what you're doing
(setq spotify-client-apple-player-status-script "
# Source: https://github.com/andrehaveman/spotify-client-node-applescript
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
(setq spotify-client-apple-player-status-script-file
      (make-temp-file "spotify-client.el" nil nil spotify-client-apple-player-status-script))

(defun spotify-client-apple-command-line (cmd)
  "Return a command line prefix for any Spotify command CMD."
  (format "%s -e %s"
          spotify-client-osascript-bin-path
          (shell-quote-argument (format "tell application \"Spotify\" to %s" cmd))))

(defun spotify-client-apple-command (cmd)
  "Send the given CMD to the Spotify client.
Return the resulting status string."
  (replace-regexp-in-string
   "\n$" ""
   (shell-command-to-string (spotify-client-apple-command-line cmd))))

(defun spotify-client-apple-set-player-status-from-process-output (process output)
  "Set the OUTPUT of the player status PROCESS to the player status."
  (spotify-client-controller-update-metadata output)
  (with-current-buffer (process-buffer process)
    (delete-region (point-min) (point-max))))

(defun spotify-client-apple-player-status ()
  "Update the player status to display the current Spotify player status."
  (let* ((process-name "spotify-client-player-status")
         (process-status (process-status process-name))
         (cmd (format "%s %s" spotify-client-osascript-bin-path spotify-client-apple-player-status-script-file)))
    (when (not process-status)
      (let* ((default-directory user-emacs-directory)
             (process (start-process-shell-command process-name "*spotify-client-player-status*" cmd)))
        (set-process-filter process 'spotify-client-apple-set-player-status-from-process-output)))))

(defun spotify-client-apple-player-state ()
  "Dispatch get player state."
  (spotify-client-apple-command "get player state"))

(defun spotify-client-apple-player-toggle-play ()
  "Dispatch playpause."
  (spotify-client-apple-command "playpause"))

(defun spotify-client-apple-player-next-track ()
  "Dispatch next track."
  (spotify-client-apple-command "next track"))

(defun spotify-client-apple-player-previous-track ()
  "Dispatch previous track."
  (spotify-client-apple-command "previous track"))

(defun spotify-client-apple-volume-up ()
  "Send message about inability to change volume."
  (message "Changing the volume not supported by the Spotify AppleScript client"))

(defun spotify-client-apple-volume-down ()
  "Send message about inability to change volume."
  (message "Changing the volume not supported by the Spotify AppleScript client"))

(defun spotify-client-apple-volume-mute-unmute ()
  "Send message about inability to change volume."
  (message "Changing the volume not supported by the Spotify AppleScript client"))

(defun spotify-client-apple-toggle-repeat ()
  "Dispatch repeat command."
  (spotify-client-apple-command "set repeating to not repeating"))

(defun spotify-client-apple-toggle-shuffle ()
  "Dispatch shuffle command."
  (spotify-client-apple-command "set shuffling to not shuffling"))

(defun spotify-client-apple-player-play-track (track-id context-id)
  "Dispatch message about playing TRACK-ID in CONTEXT-ID."
  (spotify-client-apple-command (format "play track \"%s\" in context \"%s\"" track-id context-id)))

(provide 'spotify-client-apple)
;;; spotify-client-apple.el ends here
