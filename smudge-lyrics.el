;;; smudge-lyrics.el --- Fetch lyrics for the current track  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2025 Daniel Martins

;; SPDX-License-Identifier:  GPL-3.0-or-later

;;; Commentary:

;; This library fetches lyrics for the current track using LRCLIB.

;;; Code:

(require 'smudge-controller)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'url-util)

(defvar url-http-response-status)
(defvar url-request-timeout)

(declare-function smudge-controller-player-status "smudge-controller")

(defgroup smudge-lyrics nil
  "Fetch lyrics for the current Smudge track."
  :group 'smudge)

(defcustom smudge-lyrics-auto-popup nil
  "When non-nil, fetch lyrics automatically on track changes."
  :type 'boolean
  :group 'smudge-lyrics)

(defcustom smudge-lyrics-service-url
  "https://lrclib.net/api/get"
  "Base URL for LRCLIB lyrics."
  :type 'string
  :group 'smudge-lyrics)

(defcustom smudge-lyrics-timeout 15
  "Timeout in seconds for lyrics requests."
  :type 'integer
  :group 'smudge-lyrics)

(defcustom smudge-lyrics-debug nil
  "When non-nil, log a snippet of the raw response body."
  :type 'boolean
  :group 'smudge-lyrics)

(defcustom smudge-lyrics-buffer-name "*Smudge Lyrics*"
  "Name of the buffer used to display lyrics."
  :type 'string
  :group 'smudge-lyrics)

(defvar smudge-lyrics--last-track nil
  "Cons of artist and title for the last lyrics request.")

(defvar smudge-lyrics--pending-token nil
  "Token for the latest lyrics request.")

(defvar smudge-lyrics--manual-pending nil
  "Non-nil when a manual request is waiting for metadata.")

(defvar smudge-lyrics-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `smudge-lyrics-mode'.")

(define-derived-mode smudge-lyrics-mode special-mode "Smudge-Lyrics"
  "Major mode for displaying Smudge lyrics."
  (setq buffer-read-only t)
  (setq-local buffer-offer-save nil)
  (setq-local truncate-lines nil))

(defun smudge-lyrics--clean (string)
  "Return STRING stripped of common remix annotations."
  (let ((text (string-trim string)))
    (setq text (replace-regexp-in-string " *[(\\[].*?[])]" "" text))
    (setq text (replace-regexp-in-string " *- *.*$" "" text))
    (setq text (replace-regexp-in-string " *feat\\..*$" "" text))
    (setq text (replace-regexp-in-string " *ft\\..*$" "" text))
    (string-trim text)))

(defun smudge-lyrics--track-from-metadata (metadata)
  "Return a track plist from METADATA."
  (when (hash-table-p metadata)
    (let* ((artist (gethash "artist" metadata))
           (title (gethash "name" metadata))
           (state (gethash "player_state" metadata))
           (dur-ms (gethash "duration" metadata))
           (stopped (and (stringp state) (string= state "stopped"))))
      (when (and artist title (not stopped))
        (list :artist artist
              :title title
              :duration (when (numberp dur-ms)
                          (max 1 (round (/ dur-ms 1000.0)))))))))

(defun smudge-lyrics--lrclib-url (artist title &optional duration)
  "Return LRCLIB request URL for ARTIST and TITLE."
  (let* ((params `(("artist_name" . ,artist)
                   ("track_name" . ,title)))
         (params (if duration
                     (append params `(("duration" . ,(number-to-string duration))))
                   params)))
    (concat smudge-lyrics-service-url "?"
            (mapconcat (lambda (kv)
                         (format "%s=%s" (car kv) (url-hexify-string (cdr kv))))
                       params "&"))))

(defun smudge-lyrics--parse-json-body (body)
  "Parse BODY into a hash table."
  (when (and body (not (string-empty-p (string-trim body))))
    (condition-case nil
        (json-parse-string body :object-type 'hash-table)
      (error nil))))

(defun smudge-lyrics--show-buffer (artist title lyrics &optional select)
  "Display LYRICS for ARTIST and TITLE.
When SELECT is non-nil, select the lyrics buffer."
  (let ((buffer (get-buffer-create smudge-lyrics-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s — %s\n\n" artist title))
        (insert lyrics))
      (goto-char (point-min))
      (smudge-lyrics-mode))
    (if select
        (pop-to-buffer buffer)
      (display-buffer buffer))))

(defun smudge-lyrics--handle-response (status artist title token)
  "Handle lyrics response STATUS for ARTIST and TITLE."
  (let* ((urlbuf (current-buffer))
         (select (and (listp token) (plist-get token :select)))
         (stale (and smudge-lyrics--pending-token
                     (not (equal token smudge-lyrics--pending-token)))))
    (unwind-protect
        (if stale
            (message "Lyrics response ignored (stale)")
          (let ((err (plist-get status :error)))
            (if err
                (message "Lyrics request failed: %s"
                         (if (stringp err) err (error-message-string err)))
              (with-current-buffer urlbuf
                (goto-char (point-min))
                (re-search-forward "\r?\n\r?\n" nil 'move)
                (let* ((code url-http-response-status)
                       (body (buffer-substring-no-properties (point) (point-max)))
                       (json (smudge-lyrics--parse-json-body body))
                       (lyrics (and json (gethash "plainLyrics" json))))
                  (when smudge-lyrics-debug
                    (message "Lyrics raw body: %s"
                             (truncate-string-to-width body 200 0 nil "…")))
                  (cond
                   ((eq code 404)
                    (message "No lyrics found for %s — %s" artist title))
                   ((and (numberp code) (>= code 400))
                    (message "Lyrics service error: %s" code))
                   ((and lyrics (not (string-empty-p (string-trim lyrics))))
                    (smudge-lyrics--show-buffer artist title lyrics select))
                   (t
                    (message "No lyrics returned for %s — %s" artist title))))))))
      (when (equal token smudge-lyrics--pending-token)
        (setq smudge-lyrics--pending-token nil))
      (when (buffer-live-p urlbuf)
        (kill-buffer urlbuf)))))

(defun smudge-lyrics--request (artist title duration token)
  "Request lyrics for ARTIST and TITLE using DURATION and TOKEN."
  (let* ((artist (smudge-lyrics--clean artist))
         (title (smudge-lyrics--clean title))
         (url (smudge-lyrics--lrclib-url artist title duration))
         (url-request-method "GET")
         (url-request-timeout smudge-lyrics-timeout))
    (url-retrieve url #'smudge-lyrics--handle-response
                  (list artist title token) t t)))

(defun smudge-lyrics--request-track (track &optional select)
  "Request lyrics for TRACK plist.
When SELECT is non-nil, select the lyrics buffer on success."
  (let* ((artist (plist-get track :artist))
         (title (plist-get track :title))
         (duration (plist-get track :duration))
         (token (list :id (float-time) :select select)))
    (setq smudge-lyrics--pending-token token)
    (smudge-lyrics--request artist title duration token)
    (message "Fetching lyrics for %s — %s..." artist title)))

(defun smudge-lyrics--track-id (track)
  "Return a stable identifier for TRACK."
  (cons (plist-get track :artist)
        (plist-get track :title)))

(defun smudge-lyrics--maybe-request (track &optional force select)
  "Request lyrics for TRACK unless already fetched.
When FORCE is non-nil, always request lyrics.
When SELECT is non-nil, select the lyrics buffer on success."
  (let ((id (smudge-lyrics--track-id track)))
    (when (or force (not (equal id smudge-lyrics--last-track)))
      (setq smudge-lyrics--last-track id)
      (smudge-lyrics--request-track track select))))

(defun smudge-lyrics--handle-metadata (metadata)
  "React to METADATA updates from Smudge."
  (let ((track (smudge-lyrics--track-from-metadata metadata))
        (manual-requested nil))
    (when smudge-lyrics--manual-pending
      (setq smudge-lyrics--manual-pending nil)
      (if track
          (progn
            (setq manual-requested t)
            (smudge-lyrics--maybe-request track t t))
        (message "No active track available for lyrics.")))
    (when (and smudge-lyrics-auto-popup track (not manual-requested))
      (smudge-lyrics--maybe-request track nil nil))))

;;;###autoload
(defun smudge-lyrics-popup ()
  "Fetch lyrics for the current Smudge track."
  (interactive)
  (setq smudge-lyrics--manual-pending t)
  (smudge-controller-player-status))

(add-hook 'smudge-controller-metadata-hook #'smudge-lyrics--handle-metadata)

(provide 'smudge-lyrics)
;;; smudge-lyrics.el ends here
