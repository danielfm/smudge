;; spotify-remote.el --- Spotify.el remote minor mode

;; Copyright (C) 2014-2016 Daniel Fernandes Martins

;; Code:

(defvar spotify-remote-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-p M-s") 'spotify-toggle-shuffle)
    (define-key map (kbd "M-p M-r") 'spotify-toggle-repeat)
    (define-key map (kbd "M-p M-p") 'spotify-toggle-play)
    (define-key map (kbd "M-p M-b") 'spotify-previous-track)
    (define-key map (kbd "M-p M-f") 'spotify-next-track)
    map)
  "Local keymap for `spotify-remote-mode' buffers.")

(defvar spotify-mode-line-prefix " \u266b")
(defvar spotify-mode-line spotify-mode-line-prefix)

(define-minor-mode spotify-remote-mode
  "Toggles Spotify Remote mode.
A positive prefix argument enables the mode, any other prefix
argument disables it. From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.
     
When Spotify Remote mode is enabled, it's possible to toggle
the repeating and shuffling status of the running Spotify process.
See commands \\[spotify-toggle-repeating] and
\\[spotify-toggle-shuffling]."
  :group 'spotify
  :init-value nil
  :lighter spotify-mode-line)

(defun spotify-update-mode-line (str)
  "Sets the given str to the mode line, prefixed with the mode identifier."
  (let ((normalized-str (replace-regexp-in-string "\n$" "" str)))
    (if (eq "" normalized-str)
        (setq spotify-mode-line spotify-mode-line-prefix)
      (setq spotify-mode-line (concat spotify-mode-line-prefix " " normalized-str)))
    (when (bound-and-true-p spotify-remote-mode)
      (force-mode-line-update))))

(defun turn-on-spotify-remote-mode ()
  "Turns the `spotify-remote-mode' on in the current buffer."
  (spotify-remote-mode 1))

(define-globalized-minor-mode global-spotify-remote-mode
  spotify-remote-mode turn-on-spotify-remote-mode)

(provide 'spotify-remote)
