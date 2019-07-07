;; spotify-windows.el --- Windows-specific code for Spotify.el

;; Copyright (C) 2019 Tatu Lahtela

;; Code:

(setq spotify-windows-player-status-script "")

;; Write script to a temp file
(setq spotify-windows-player-status-script-file
      (make-temp-file "spotify.el" nil nil spotify-windows-player-status-script))

(defun spotify-windows-command-line (cmd)
  (message "Got %s" cmd)
  )

(defun spotify-windows-command (cmd)
  "Sends the given command to the Spotify client and returns the resulting
   status string."
  (replace-regexp-in-string
   "\n$" ""
   (shell-command-to-string (spotify-windows-command-line cmd))))

(defun spotify-windows-set-mode-line-from-process-output (process output)
  "Sets the output of the player status process to the mode line."
  (spotify-replace-mode-line-flags output)
  (with-current-buffer (process-buffer process)
    (delete-region (point-min) (point-max))))

(defun spotify-windows-player-status ()
  "Updates the mode line to display the current Spotify player status."
  )

(defun spotify-windows-player-state ()
  (spotify-windows-command "get player state"))

(defun spotify-windows-player-toggle-play ()
  (spotify-windows-command "playpause"))

(defun spotify-windows-player-next-track ()
  (spotify-windows-command "next track"))

(defun spotify-windows-player-previous-track ()
  (spotify-windows-command "previous track"))

(defun spotify-windows-is-shuffling-supported ()
  t)

(defun spotify-windows-is-repeating-supported ()
  t)

(defun spotify-windows-toggle-repeat ()
  (spotify-windows-command "set repeating to not repeating"))

(defun spotify-windows-toggle-shuffle ()
  (spotify-windows-command "set shuffling to not shuffling"))

(defun spotify-windows-is-repeating ()
  (string= "true" (spotify-windows-command "repeating")))

(defun spotify-windows-is-shuffling ()
  (string= "true" (spotify-windows-command "shuffling")))

(defun spotify-windows-player-play-track (track-id context-id)
  (spotify-windows-command (format "play track \"%s\" in context \"%s\"" track-id context-id)))

(defun spotify-windows-player-pause ()
  (spotify-windows-command "pause"))

(provide 'spotify-windows)
