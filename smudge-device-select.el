;;; smudge-device-select.el --- Smudge device selection major mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jason Dufair

;;; Commentary:

;; This library implements methods, UI, and a minor mode to use the "connect" RESTful APIs to manage
;; and query Spotify clients on the network.

;;; Code:

(require 'smudge-api)
(require 'smudge-controller)

(defcustom smudge-selected-device-id ""
  "The id of the device selected for transport."
  :group 'smudge
  :type 'string)

(defvar smudge-device-select-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "g")     'smudge-device-select-update)
    map)
  "Local keymap for `smudge-device-select-mode' buffers.")

(define-derived-mode smudge-device-select-mode tabulated-list-mode "Device-Select"
  "Major mode for selecting a Spotify Connect device for transport.")

(defun smudge-device-select-update ()
  "Fetches the list of devices using the device list endpoint."
  (interactive)
  (let ((buffer (current-buffer)))
    (smudge-api-device-list
     (lambda (json)
       (if-let ((devices (gethash 'devices json))
                (line (string-to-number (format-mode-line "%l"))))
           (progn
             (pop-to-buffer buffer)
             (smudge-devices-print devices)
             (goto-char (point-min))
             (forward-line (1- line))
             (message "Device list updated."))
         (message "No devices are available."))))))

(defun smudge-device-select-active ()
  "Set the selected device to the active device per the API."
  (smudge-api-device-list
   (lambda (json)
     (when-let ((devices (gethash 'devices json)))
       (while (let* ((device (car devices))
                     (is-active (smudge-device-get-device-is-active device)))
                (progn
                  (when (and device is-active)
                    (progn
                      (setq smudge-selected-device-id (smudge-device-get-device-id device))
                      (smudge-controller-player-status)))
                  (setq devices (cdr devices))
                  (and device (not is-active)))))))))

(defun smudge-devices-print (devices)
  "Append the given DEVICES to the devices view."
  (let (entries)
    (dolist (device devices)
      (let ((name (smudge-device-get-device-name device))
            (is-active (smudge-device-get-device-is-active device))
            (is-restricted (smudge-device-get-device-is-restricted device))
            (volume (smudge-device-get-device-volume device))
            (device-id (smudge-device-get-device-id device)))
        (unless is-restricted
          (push
           (list device
                 (vector
                  (cons name
                        (list 'face 'link
                              'follow-link t
                              'action `(lambda (_)
                                         (smudge-api-transfer-player
                                          ,device-id
                                          (lambda (json)
                                            (setq smudge-selected-device-id ,device-id)
                                            (message "Device '%s' selected" ,name))))
                              'help-echo (format "Select '%s' for transport" name)))
                  (if is-active "X" "")
                  (if is-active (number-to-string volume) "")))
           entries))))
    (setq-local tabulated-list-entries nil)
    (setq-local tabulated-list-entries (append tabulated-list-entries (nreverse entries)))
    (smudge-device-set-list-format)
    (tabulated-list-init-header)
    (tabulated-list-print t)))

(defun smudge-device-set-list-format ()
  "Configures the column data for the device view."
  (setq tabulated-list-format
        (vector `("Device" ,(- (window-width) 22) t)
                '("Active" 12 t)
                '("Volume" 8 nil :right-align t))))

(defun smudge-device-get-device-name (device)
  "Return the name from the given DEVICE hash."
  (gethash 'name device))

(defun smudge-device-get-device-is-active (device)
  "Return whether the DEVICE is currently playing content."
  (eq (and device (gethash 'is_active device)) t))

(defun smudge-device-get-device-volume (device)
  "Return the volume of the DEVICE."
  (gethash 'volume_percent device))

(defun smudge-device-get-device-is-restricted (device)
  "Return whether the DEVICE can receive commands."
  (eq (gethash 'is_restricted device) t))

(defun smudge-device-get-device-id (device)
  "Return the unique id of DEVICE."
  (gethash 'id device))

(provide 'smudge-device-select)
;;; smudge-device-select.el ends here
