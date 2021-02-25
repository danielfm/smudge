;; spotemacs-remote.el --- Spotemacs remote minor mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Daniel Fernandes Martins

;;; Commentary:

;; This library implements a minor mode that allows for interacting with Spotify and displaying
;; transport status.  This also includes a global minor mode definition.

;;; Code:

(require 'spotemacs-controller)
(require 'spotemacs-device-select)

(defcustom spotemacs-keymap-prefix nil
  "Spotemacs remote keymap prefix."
  :group 'spotemacs
  :type 'string)

(defcustom spotemacs-status-location 'modeline
  "Specify where to show the player status: one of '(modeline title-bar nil)."
  :type '(choice (const :tag "Modeline" modeline)
                 (const :tag "Title Bar" title-bar)
                 (const :tag "Do not show" nil))
  :group 'spotemacs)

(defcustom spotemacs-title-bar-separator "    "
  "String used to separate player status from the remaining text on the title bar."
  :type 'string
  :group 'spotemacs)

(defun spotemacs-remote-add-frame-title-status (status title-section)
  "Return the TITLE-SECTION or part thereof with the player STATUS appended."
  (if (stringp title-section)
      (cons title-section (list status))
    (append title-section (list status))))

(defun spotemacs-remote-set-frame-title (status)
  "Parse and set the frame title, appending STATUS to all frame scenarios."
  (spotemacs-remote-remove-status-from-frame-title status)
  (setq frame-title-format
        (if (and (listp frame-title-format) (eq (car frame-title-format) 'multiple-frames))
            (let ((multiple-frame-format (car (nthcdr 1 frame-title-format)))
                  (single-frame-format (car (nthcdr 2 frame-title-format))))
              (list 'multiple-frames
                    (spotemacs-remote-add-frame-title-status status multiple-frame-format)
                    (spotemacs-remote-add-frame-title-status status single-frame-format)))
          (spotemacs-remote-add-frame-title-status status frame-title-format))))

(defun spotemacs-remote-remove-status-from-frame-title (status)
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

(defvar spotemacs-mode-map
  (let ((map (make-sparse-keymap)))
    (when spotemacs-keymap-prefix
      (define-key map spotemacs-keymap-prefix 'spotemacs-command-map))
    map)
  "Keymap for Spotemacs remote mode.")

(define-minor-mode global-spotemacs-remote-mode
  "Toggles Spotemacs Remote mode.
A positive prefix argument enables the mode, any other prefix
argument disables it. From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When Spotemacs Remote mode is enabled, it's possible to toggle
the repeating and shuffling status of the running Spotify process.
See commands \\[spotemacs-toggle-repeating] and
\\[spotemacs-toggle-shuffling]."
  :group 'spotemacs
  :init-value nil
  :global t
  :keymap spotemacs-mode-map
  (let ((s `(,spotemacs-title-bar-separator (:eval (spotemacs-remote-player-status-text)))))
    (if global-spotemacs-remote-mode
        (progn
          (spotemacs-controller-start-player-status-timer)
          (cond ((or (eq spotemacs-status-location 'modeline) (not (display-graphic-p)))
                 (unless (member s global-mode-string)
                   (push s global-mode-string)))
                ((eq spotemacs-status-location 'title-bar)
                 (spotemacs-remote-set-frame-title s)))
          (when (eq spotemacs-transport 'connect)
            (spotemacs-device-select-active)))
      (progn
        (spotemacs-controller-stop-player-status-timer)
        (cond ((or (eq spotemacs-status-location 'modeline) (not (display-graphic-p)))
               (when (member s global-mode-string)
                 (setq global-mode-string (remove s global-mode-string))))
              ((eq spotemacs-status-location 'title-bar)
               (spotemacs-remote-remove-status-from-frame-title s)))))))

(defvar spotemacs-remote-player-status-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mode-line> <mouse-1>")
      'spotemacs-remote-popup-menu)
    map)
  "Keymap for Spotemacs mode-line status.")

(defun spotemacs-remote-update-player-status (str)
  "Set the given STR to the player status, prefixed with the mode identifier."
  (when (not (string= str spotemacs-controller-player-status))
    (setq spotemacs-controller-player-status str)
    (force-mode-line-update t)))

(defun spotemacs-remote-player-status-text ()
  "Return the propertized text to be displayed as the lighter."
  (propertize spotemacs-controller-player-status
              'keymap spotemacs-remote-player-status-map
              'help-echo "mouse-1: Show spotemacs.el menu"
              'mouse-face 'mode-line-highlight))

(provide 'spotemacs-remote)
;;; spotemacs-remote.el ends here
