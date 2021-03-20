;; smudge-remote.el --- Smudge remote minor mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Daniel Fernandes Martins

;;; Commentary:

;; This library implements a minor mode that allows for interacting with Spotify and displaying
;; transport status.  This also includes a global minor mode definition.

;;; Code:

(require 'smudge-controller)
(require 'smudge-device-select)

(defcustom smudge-keymap-prefix nil
  "Smudge remote keymap prefix."
  :group 'smudge
  :type 'string)

(defcustom smudge-status-location 'modeline
  "Specify where to show the player status: one of '(modeline title-bar nil)."
  :type '(choice (const :tag "Modeline" modeline)
                 (const :tag "Title Bar" title-bar)
                 (const :tag "Do not show" nil))
  :group 'smudge)

(defcustom smudge-title-bar-separator "    "
  "String used to separate player status from the remaining text on the title bar."
  :type 'string
  :group 'smudge)

(defun smudge-remote-add-frame-title-status (status title-section)
  "Return the TITLE-SECTION or part thereof with the player STATUS appended."
  (if (stringp title-section)
      (cons title-section (list status))
    (append title-section (list status))))

(defun smudge-remote-set-frame-title (status)
  "Parse and set the frame title, appending STATUS to all frame scenarios."
  (smudge-remote-remove-status-from-frame-title status)
  (setq frame-title-format
        (if (and (listp frame-title-format) (eq (car frame-title-format) 'multiple-frames))
            (let ((multiple-frame-format (car (nthcdr 1 frame-title-format)))
                  (single-frame-format (car (nthcdr 2 frame-title-format))))
              (list 'multiple-frames
                    (smudge-remote-add-frame-title-status status multiple-frame-format)
                    (smudge-remote-add-frame-title-status status single-frame-format)))
          (smudge-remote-add-frame-title-status status frame-title-format))))

(defun smudge-remote-remove-status-from-frame-title (status)
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

(defvar smudge-mode-map
  (let ((map (make-sparse-keymap)))
    (when smudge-keymap-prefix
      (define-key map smudge-keymap-prefix 'smudge-command-map))
    map)
  "Keymap for Smudge remote mode.")

(define-minor-mode global-smudge-remote-mode
  "Toggles Smudge Remote mode.
A positive prefix argument enables the mode, any other prefix
argument disables it. From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When Smudge Remote mode is enabled, it's possible to toggle
the repeating and shuffling status of the running Spotify process.
See commands \\[smudge-toggle-repeating] and
\\[smudge-toggle-shuffling]."
  :group 'smudge
  :init-value nil
  :global t
  :keymap smudge-mode-map
  (let ((s `(,smudge-title-bar-separator (:eval (smudge-remote-player-status-text)))))
    (if global-smudge-remote-mode
        (progn
          (smudge-controller-start-player-status-timer)
          (cond ((or (eq smudge-status-location 'modeline) (not (display-graphic-p)))
                 (unless (member s global-mode-string)
                   (push s global-mode-string)))
                ((eq smudge-status-location 'title-bar)
                 (smudge-remote-set-frame-title s)))
          (when (eq smudge-transport 'connect)
            (smudge-device-select-active)))
      (progn
        (smudge-controller-stop-player-status-timer)
        (cond ((or (eq smudge-status-location 'modeline) (not (display-graphic-p)))
               (when (member s global-mode-string)
                 (setq global-mode-string (remove s global-mode-string))))
              ((eq smudge-status-location 'title-bar)
               (smudge-remote-remove-status-from-frame-title s)))))))

(defvar smudge-remote-player-status-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mode-line> <mouse-1>")
      'smudge-remote-popup-menu)
    map)
  "Keymap for Smudge mode-line status.")

(defun smudge-remote-update-player-status (str)
  "Set the given STR to the player status, prefixed with the mode identifier."
  (when (not (string= str smudge-controller-player-status))
    (setq smudge-controller-player-status str)
    (force-mode-line-update t)))

(defun smudge-remote-player-status-text ()
  "Return the propertized text to be displayed as the lighter."
  (propertize smudge-controller-player-status
              'keymap smudge-remote-player-status-map
              'help-echo "mouse-1: Show smudge.el menu"
              'mouse-face 'mode-line-highlight))

(provide 'smudge-remote)
;;; smudge-remote.el ends here
