;; spotify-apple.el --- Apple-specific code for Spotify.el

;; Copyright (C) 2014-2016 Daniel Fernandes Martins

;; Code:

(defcustom spotify-osascript-bin-path "/usr/bin/osascript"
  "Path to `osascript' binary."
  :type 'string)

;; Do not change this unless you know what you're doing
(defvar spotify-player-status-script "
tell application \"Spotify\"
  set playerState     to get player state as string

  # Empty data in order to avoid returning an error
  if (playerState = \"stopped\") then
    return \"\n\n\n\n\n\nstopped\n\"
  end if

  set trackId         to id of current track as string
  set trackArtist     to artist of current track as string
  set trackName       to name of current track as string
  set trackNumber     to track number of current track as string
  set trackDiscNumber to disc number of current track as string
  set trackDuration   to duration of current track as string

  set playerPosition  to get player position as string

  return trackId & \"\n\" & trackArtist & \"\n\" & trackName & \"\n\" & trackNumber & \"\n\" & trackDiscNumber & \"\n\" & trackDuration & \"\n\" & playerState & \"\n\" & playerPosition
end tell")

(defun spotify-apple-command-line (cmd)
  "Returns a command line prefix for any Spotify command."
  (format "%s -e 'tell application \"Spotify\" to %s'" spotify-osascript-bin-path cmd))

(defun spotify-apple-command (cmd)
  "Sends the given command to the Spotify client and returns the resulting
   status string."
  (replace-regexp-in-string
   "\n$" ""
   (shell-command-to-string (spotify-apple-command-line cmd))))

(defun spotify-apple-player-status-command-line ()
  "Returns the current Spotify player status that is set to the mode line."
  (format "echo %s | %s"
          (shell-quote-argument spotify-player-status-script)
          spotify-osascript-bin-path))

(defun spotify-apple-set-mode-line-from-process-output (process output)
  "Sets the output of the player status process to the mode line."
  (spotify-replace-mode-line-flags output)
  (with-current-buffer (process-buffer process)
    (delete-region (point-min) (point-max))))

(defun spotify-apple-player-status ()
  "Updates the mode line to display the current Spotify player status."
  (let* ((process-name "spotify-player-status")
         (process-status (process-status process-name)))
    (if (and (spotify-connected-p) (not process-status))
        (let ((process (start-process-shell-command process-name "*spotify-player-status*" (spotify-apple-player-status-command-line))))
          (set-process-filter process 'spotify-apple-set-mode-line-from-process-output))
      (spotify-update-mode-line ""))))

(defun spotify-apple-player-state ()
  (spotify-apple-command "get player state"))

(defun spotify-apple-player-toggle-play ()
  (spotify-apple-command "playpause"))

(defun spotify-apple-player-next-track ()
  (spotify-apple-command "next track"))

(defun spotify-apple-player-previous-track ()
  (spotify-apple-command "previous track"))

(defun spotify-apple-toggle-repeat ()
  (spotify-apple-command "set repeating to not repeating"))

(defun spotify-apple-toggle-shuffle ()
  (spotify-apple-command "set shuffling to not shuffling"))

(defun spotify-apple-player-play-track (track-id context-id)
  (spotify-apple-command (format "play track \"%s\" in context \"%s\"" track-id context-id)))

(defun spotify-apple-player-pause ()
  (spotify-apple-command "pause"))

(provide 'spotify-apple)
