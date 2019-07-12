;; spotify-web.el --- Web-specific code for Spotify.el

;; Copyright (C) 2019 Tatu Lahtela

;; Code:

(setq spotify-web-player-status-script "")

;; Write script to a temp file
(setq spotify-web-player-status-script-file
      (make-temp-file "spotify.el" nil nil spotify-web-player-status-script))

(defun spotify-web-command-line (cmd)
  (message "Got %s" cmd)
  )

(defun spotify-web-command (cmd)
  "Sends the given command to the Spotify client and returns the resulting
   status string."
  (replace-regexp-in-string
   "\n$" ""
   (shell-command-to-string (spotify-web-command-line cmd))))

(defun spotify-web-set-mode-line-from-process-output (process output)
  "Sets the output of the player status process to the mode line."
  (spotify-replace-mode-line-flags output)
  (with-current-buffer (process-buffer process)
    (delete-region (point-min) (point-max))))

(defun spotify-web-player-status ()
  "Updates the mode line to display the current Spotify player status."
  (message "modeline status called")
  )

(defun spotify-web-player-state ()
)

(defun spotify-web-player-toggle-play ()
  (spotify-api-play)
)

(defun spotify-web-player-next-track ()
  (spotify-api-next))

(defun spotify-web-player-previous-track ()
    (spotify-api-previous))

(defun spotify-web-is-shuffling-supported ()
  t)

(defun spotify-web-is-repeating-supported ()
  t)

(defun spotify-web-toggle-repeat ()
)

(defun spotify-web-toggle-shuffle ()
)

(defun spotify-web-is-repeating ()
)

(defun spotify-web-is-shuffling ()
)

(defun spotify-web-player-play-track (track-id context-id)
)

(defun spotify-web-player-play ()
    (spotify-api-play))
  

(defun spotify-web-player-pause ()
    (spotify-api-pause))

(provide 'spotify-web)
