;; spotify-client-remote.el --- spotify-client remote minor mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Daniel Fernandes Martins

;;; Commentary:

;; This library implements a minor mode that allows for interacting with Spotify and displaying
;; transport status.  This also includes a global minor mode definition.

;;; Code:

(require 'spotify-client-controller)
(require 'spotify-client-device-select)

(defcustom spotify-client-keymap-prefix nil
  "Spotify-Client remote keymap prefix."
  :group 'spotify-client
  :type 'string)

(defcustom spotify-client-status-location 'modeline
  "Specify where to show the player status: one of '(modeline title-bar nil)."
  :type '(choice (const :tag "Modeline" modeline)
                 (const :tag "Title Bar" title-bar)
                 (const :tag "Do not show" nil))
  :group 'spotify-client)

(defcustom spotify-client-title-bar-separator "    "
  "String used to separate player status from the remaining text on the title bar."
  :type 'string
  :group 'spotify-client)

(defun spotify-client-remote-add-frame-title-status (status title-section)
  "Return the TITLE-SECTION or part thereof with the player STATUS appended."
  (if (stringp title-section)
      (cons title-section (list status))
    (append title-section (list status))))

(defun spotify-client-remote-set-frame-title (status)
  "Parse and set the frame title, appending STATUS to all frame scenarios."
  (spotify-client-remote-remove-status-from-frame-title status)
  (setq frame-title-format
        (if (and (listp frame-title-format) (eq (car frame-title-format) 'multiple-frames))
            (let ((multiple-frame-format (car (nthcdr 1 frame-title-format)))
                  (single-frame-format (car (nthcdr 2 frame-title-format))))
              (list 'multiple-frames
                    (spotify-client-remote-add-frame-title-status status multiple-frame-format)
                    (spotify-client-remote-add-frame-title-status status single-frame-format)))
          (spotify-client-remote-add-frame-title-status status frame-title-format))))

(defun spotify-client-remote-remove-status-from-frame-title (status)
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

(defvar spotify-client-mode-map
  (let ((map (make-sparse-keymap)))
    (when spotify-client-keymap-prefix
      (define-key map spotify-client-keymap-prefix 'spotify-client-command-map))
    map)
  "Keymap for Spotify-Client remote mode.")

(define-minor-mode global-spotify-client-remote-mode
  "Toggles Spotify-Client Remote mode.
A positive prefix argument enables the mode, any other prefix
argument disables it. From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When Spotify-Client Remote mode is enabled, it's possible to toggle
the repeating and shuffling status of the running Spotify process.
See commands \\[spotify-client-toggle-repeating] and
\\[spotify-client-toggle-shuffling]."
  :group 'spotify-client
  :init-value nil
  :global t
  :keymap spotify-client-mode-map
  (let ((s `(,spotify-client-title-bar-separator (:eval (spotify-client-remote-player-status-text)))))
    (if global-spotify-client-remote-mode
        (progn
          (spotify-client-controller-start-player-status-timer)
          (cond ((or (eq spotify-client-status-location 'modeline) (not (display-graphic-p)))
                 (unless (member s global-mode-string)
                   (push s global-mode-string)))
                ((eq spotify-client-status-location 'title-bar)
                 (spotify-client-remote-set-frame-title s)))
          (when (eq spotify-client-transport 'connect)
            (spotify-client-device-select-active)))
      (progn
        (spotify-client-controller-stop-player-status-timer)
        (cond ((or (eq spotify-client-status-location 'modeline) (not (display-graphic-p)))
               (when (member s global-mode-string)
                 (setq global-mode-string (remove s global-mode-string))))
              ((eq spotify-client-status-location 'title-bar)
               (spotify-client-remote-remove-status-from-frame-title s)))))))

(defvar spotify-client-remote-player-status-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mode-line> <mouse-1>")
      'spotify-client-remote-popup-menu)
    map)
  "Keymap for Spotify-Client mode-line status.")

(defun spotify-client-remote-update-player-status (str)
  "Set the given STR to the player status, prefixed with the mode identifier."
  (when (not (string= str spotify-client-controller-player-status))
    (setq spotify-client-controller-player-status str)
    (force-mode-line-update t)))

(defun spotify-client-remote-player-status-text ()
  "Return the propertized text to be displayed as the lighter."
  (propertize spotify-client-controller-player-status
              'keymap spotify-client-remote-player-status-map
              'help-echo "mouse-1: Show spotify-client.el menu"
              'mouse-face 'mode-line-highlight))

(provide 'spotify-client-remote)
;;; spotify-client-remote.el ends here
