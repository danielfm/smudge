;;; package --- Summary

;;; Commentary:

;; spotify-device-select.el --- Spotify.el device selection major mode

;; Copyright (C) 2019 Jason Dufair

;;; Code:

(require 'spotify-api)

(defcustom spotify-selected-device-id ""
  "The id of the device selected for transport."
  :type 'string)

(defvar spotify-device-select-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "g")     'spotify-device-select-update)
    map)
  "Local keymap for `spotify-device-select-mode' buffers.")

(define-derived-mode spotify-device-select-mode tabulated-list-mode "Device-Select"
  "Major mode for selecting a Spotify Connect device for transport.")

(defun spotify-device-select-update ()
  "Fetches the list of devices using the device list endpoint."
  (interactive)
  (lexical-let ((buffer (current-buffer)))
    (spotify-api-device-list
     (lambda (json)
       (if-let ((devices (gethash 'devices json))
                (line (string-to-number (format-mode-line "%l"))))
           (progn
             (pop-to-buffer buffer)
             (spotify-devices-print devices)
             (goto-char (point-min))
             (forward-line (1- line))
             (message "Device list updated."))
         (message "No devices are available."))))))

(defun spotify-devices-print (devices)
  "Append the given DEVICES to the devices view."
  (let (entries)
    (dolist (device devices)
      (let ((name (spotify-get-device-name device))
            (is-active (spotify-get-device-is-active device))
            (is-restricted (spotify-get-device-is-restricted device))
            (volume (spotify-get-device-volume device))
            (device-id (spotify-get-device-id device)))
        (unless is-restricted
          (push
           (list device
                 (vector
                  (cons name
                        (list 'face 'link
                              'follow-link t
                              'action `(lambda (_)
                                         (spotify-api-transfer-player
                                          ,device-id
                                          (lambda (json)
                                            (setq spotify-selected-device-id ,device-id)
                                            (message "Device '%s' selected" ,name))))
                              'help-echo (format "Select '%s' for transport" name)))
                  (if is-active "X" "")
                  (if is-active (number-to-string volume) "")))
           entries))))
    (setq-local tabulated-list-entries nil)
    (setq-local tabulated-list-entries (append tabulated-list-entries (nreverse entries)))
    (spotify-device-set-list-format)
    (tabulated-list-init-header)
    (tabulated-list-print t)))

(defun spotify-device-set-list-format ()
  "Configures the column data for the device view."
  (setq tabulated-list-format
        (vector `("Device" ,(- (window-width) 22) t)
                '("Active" 12 t)
                '("Volume" 8 nil :right-align t))))

(defun spotify-get-device-name (device)
  "Return the name from the given DEVICE hash."
  (gethash 'name device))

(defun spotify-get-device-is-active (device)
  "Return whether the DEVICE is currently playing content."
  (eq (gethash 'is_active device) t))

(defun spotify-get-device-volume (device)
  "Return the volume of the DEVICE."
  (gethash 'volume_percent device))

(defun spotify-get-device-is-restricted (device)
  "Return whether the DEVICE can receive commands."
  (eq (gethash 'is_restricted device) t))

(defun spotify-get-device-id (device)
  "Return the unique id of DEVICE."
  (gethash 'id device))

(provide 'spotify-device-select)

;;; spotify-device-select.el ends here
