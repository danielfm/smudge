;; spotify-apple.el --- Apple-specific code for Spotify.el

;; Copyright (C) 2014-2019 Daniel Fernandes Martins

;; Code:

(defcustom spotify-osascript-bin-path "/usr/bin/osascript"
  "Path to `osascript' binary."
  :type 'string)

; Do not change this unless you know what you're doing
(setq spotify-apple-player-status-script "
# Source: https://github.com/andrehaveman/spotify-node-applescript
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
(setq spotify-apple-player-status-script-file
      (make-temp-file "spotify.el" nil nil spotify-apple-player-status-script))

(defun spotify-apple-command-line (cmd)
  "Returns a command line prefix for any Spotify command."
  (format "%s -e %s"
          spotify-osascript-bin-path
          (shell-quote-argument (format "tell application \"Spotify\" to %s" cmd))))

(defun spotify-apple-command (cmd)
  "Sends the given command to the Spotify client and returns the resulting
   status string."
  (replace-regexp-in-string
   "\n$" ""
   (shell-command-to-string (spotify-apple-command-line cmd))))

(defun spotify-apple-set-mode-line-from-process-output (process output)
  "Sets the output of the player status process to the mode line."
  (spotify-replace-mode-line-flags output)
  (with-current-buffer (process-buffer process)
    (delete-region (point-min) (point-max))))

(defun spotify-apple-player-status ()
  "Updates the mode line to display the current Spotify player status."
  (let* ((process-name "spotify-player-status")
         (process-status (process-status process-name))
         (cmd (format "%s %s" spotify-osascript-bin-path spotify-apple-player-status-script-file)))
    (when (not process-status)
      (let* ((default-directory user-emacs-directory)
             (process (start-process-shell-command process-name "*spotify-player-status*" cmd)))
        (set-process-filter process 'spotify-apple-set-mode-line-from-process-output)))))

(defun spotify-apple-player-state ()
  (spotify-apple-command "get player state"))

(defun spotify-apple-player-toggle-play ()
  (spotify-apple-command "playpause"))

(defun spotify-apple-player-next-track ()
  (spotify-apple-command "next track"))

(defun spotify-apple-player-previous-track ()
  (spotify-apple-command "previous track"))

(defun spotify-apple-volume-up ()
  (message "Changing the volume not supported by the Spotify AppleScript client"))

(defun spotify-apple-volume-down ()
  (message "Changing the volume not supported by the Spotify AppleScript client"))

(defun spotify-apple-volume-mute-unmute ()
  (message "Changing the volume not supported by the Spotify AppleScript client"))

(defun spotify-apple-toggle-repeat ()
  (spotify-apple-command "set repeating to not repeating"))

(defun spotify-apple-toggle-shuffle ()
  (spotify-apple-command "set shuffling to not shuffling"))

(defun spotify-apple-player-play-track (track-id context-id)
  (spotify-apple-command (format "play track \"%s\" in context \"%s\"" track-id context-id)))

(defun spotify-apple-player-pause ()
  (spotify-apple-command "pause"))

(provide 'spotify-apple)
