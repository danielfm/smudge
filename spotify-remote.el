;; spotify-remote.el --- Spotify.el remote minor mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Daniel Fernandes Martins

;;; Commentary:

;; This library implements a minor mode that allows for interacting with Spotify and displaying
;; transport status.  This also includes a global minor mode definition.

;;; Code:

(require 'spotify-controller)

(defcustom spotify-keymap-prefix nil
  "Spotify remote keymap prefix."
  :group 'spotify
  :type 'string)

(defcustom spotify-status-location 'modeline
  "Specify where to show the player status: one of '(modeline title-bar nil)."
  :type '(choice (const :tag "Modeline" modeline)
                 (const :tag "Title Bar" title-bar)
                 (const :tag "Do not show" nil))
  :group 'spotify)

(defcustom spotify-title-bar-separator "    "
  "The string used to separate the player status from the remaining text on the title bar."
  :type 'string
  :group 'spotify)

(defun spotify-add-frame-title-status (status title-section)
  "Return the TITLE-SECTION or part thereof with the player STATUS appended."
  (if (stringp title-section)
      (cons title-section (list status))
    (append title-section (list status))))

(defun spotify-set-frame-title (status)
  "Parse and set the frame title, appending STATUS to all frame scenarios."
  (spotify-remove-status-from-frame-title status)
  (setq frame-title-format
        (if (and (listp frame-title-format) (eq (car frame-title-format) 'multiple-frames))
            (let ((multiple-frame-format (car (nthcdr 1 frame-title-format)))
                  (single-frame-format (car (nthcdr 2 frame-title-format))))
              (list 'multiple-frames
                    (spotify-add-frame-title-status status multiple-frame-format)
                    (spotify-add-frame-title-status status single-frame-format)))
          (spotify-add-frame-title-status status frame-title-format))))

(defun spotify-remove-status-from-frame-title (status)
  "Parse the frame title and remove the player STATUS."
  (setq frame-title-format
        (if (not (listp frame-title-format))
            frame-title-format
          (if (member status frame-title-format)
              (remove status frame-title-format)
            (mapcar (lambda (section)
                      (if (and (listp section) (member status section))
                          (remove status section)
                        section))
                    frame-title-format)))))

(defvar spotify-mode-map
  (let ((map (make-sparse-keymap)))
    (when spotify-keymap-prefix
      (define-key map spotify-keymap-prefix 'spotify-command-map))
    map)
  "Keymap for Spotify remote mode.")

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
  :keymap spotify-mode-map
  (let ((s `(,spotify-title-bar-separator (:eval (spotify-player-status-text)))))
    (if spotify-remote-mode
        (progn
          (spotify-start-player-status-timer)
          (cond ((or (eq spotify-status-location 'modeline) (not (display-graphic-p)))
                 (unless (member s global-mode-string)
                   (push s global-mode-string)))
                ((eq spotify-status-location 'title-bar)
                 (spotify-set-frame-title s))))
      (progn
        (spotify-stop-player-status-timer)
        (cond ((or (eq spotify-status-location 'modeline) (not (display-graphic-p)))
               (when (member s global-mode-string)
                 (setq global-mode-string (remove s global-mode-string))))
              ((eq spotify-status-location 'title-bar)
               (spotify-remove-status-from-frame-title s))))))
  (when (eq spotify-transport 'connect)
    (spotify-device-select-active)))

(defvar spotify-remote-player-status-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mode-line> <mouse-1>")
      'spotify-remote-popup-menu)
    map)
  "Keymap for Spotify mode-line status.")

(defun spotify-player-status-text ()
  "Return the propertized text to be displayed as the lighter."
  (propertize spotify-player-status
              'keymap spotify-remote-player-status-map
              'help-echo "mouse-1: Show Spotify.el menu"
              'mouse-face 'mode-line-highlight))

(defun turn-on-spotify-remote-mode ()
  "Turn the `spotify-remote-mode' on in the current buffer."
  (spotify-remote-mode 1))

(define-globalized-minor-mode global-spotify-remote-mode
  spotify-remote-mode turn-on-spotify-remote-mode
  :group 'spotify)

(provide 'spotify-remote)
;;; spotify-remote.el ends here
