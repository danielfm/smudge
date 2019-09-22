;;; package --- Summary

;;; Commentary:

;; spotify-device-select.el --- Spotify.el device selection major mode

;; Copyright (C) 2019 Jason Dufair

;;; Code:

(require 'spotify-api)

(defvar spotify-device-select-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "M-RET") 'spotify-device-select)
    (define-key map (kbd "g")     'spotify-device-list-reload)
    map)
  "Local keymap for `spotify-device-select-mode' buffers.")

(define-derived-mode spotify-device-select-mode tabulated-list-mode "Device-Select"
  "Major mode for selecting a Spotify Connect device for transport.")

(defun spotify-device-select-update (current-page)
  "Fetches the CURRENT-PAGE of devices using the device list endpoint."
  (let* ((json (spotify-api-device-list))
         (devices (gethash 'devices json)))
    (if devices
        (progn
          (spotify-devices-print devices current-page)
          (message "device list updated"))
      (message "No more devices"))))

(defun spotify-device-list-reload ()
  "Reloads the devices for the current device list view."
  (interactive)
	(spotify-device-select-update 1))

(defun spotify-devices-print (devices current-page)
  "Append the given DEVICES the CURRENT-PAGE devices view."
  (let (entries)
    (dolist (device devices)
      (let ((name (spotify-get-device-name device))
						(is-active (spotify-get-device-is-active device))
						(is-restricted (spotify-get-device-is-restricted device))
						(volume (spotify-get-device-volume device)))
				(unless is-restricted
					(push (list device
                    (vector
                        (cons name
                           (list 'face 'link
                                 'follow-link t
                                 'action `(lambda (_) (message "hello"))
                                 'help-echo (format "Select %s for transport" name)))
												(if is-active "🔈" "")
												(if is-active (number-to-string volume) "")))
              entries))))
    (when (eq 1 current-page)
      (setq-local tabulated-list-entries nil))
    (setq-local tabulated-list-entries (append tabulated-list-entries (nreverse entries)))
    (setq-local spotify-current-page current-page)
    (spotify-device-set-list-format)
    (tabulated-list-init-header)
    (tabulated-list-print t)))

(defun spotify-device-set-list-format ()
  "Configures the column data for the device view."
  (setq tabulated-list-format
        (vector `("Device" ,(- (window-width) 18) t)
                '("Active" 8 t)
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

(provide 'spotify-device-select)

;;; spotify-device-select.el ends here
