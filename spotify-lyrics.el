;;; package --- Summary -*- lexical-binding: t -*-

;;; Commentary:

;; spotify-lyrics.el --- Spotify.el interface for Genius lyrics

;; Copyright (C) 2019 Jason Dufair

;;; Code:

(require 'json)
(require 'spotify-api)
(require 'oauth2)

(defcustom spotify-genius-token ""
  "The OAuth token generated at genius.com."
  :group 'spotify
  :type 'string)

(defcustom spotify-lyrics-split-window t
  "When truthy, display the lyrics in a split window.  Otherwise display in current window."
  :group 'spotify
  :type 'boolean)

(defconst spotify-genius-endpoint "https://api.genius.com")
(defconst spotify-genius-website-url "https://genius.com")

(defun spotify-display-lyrics (metadata)
  "Display lyrics based on currently playing track in METADATA."
  (spotify-genius-search
   metadata
   (lambda (lyrics-url)
     (if spotify-lyrics-split-window
         (let ((window (get-buffer-window "*eww*")))
           (if window
               (select-window window)
             (progn
               (split-window-sensibly)
               (other-window 1)))))
     (eww (concat spotify-genius-website-url lyrics-url)))))

(defun spotify-genius-search (metadata callback)
  "Search the genius API using METADATA.  Call CALLBACK with the URL of the lyrics."
  (spotify-api-call-endpoint-async
   "GET"
   (concat "/search?"
           (url-build-query-string
            `((q      ,(concat (gethash 'artist metadata) (gethash 'name metadata))))
            nil t))
   (make-oauth2-token :access-token spotify-genius-token)
   spotify-genius-endpoint
   nil
   (lambda (json)
     (let* ((hits (gethash 'hits (gethash 'response json)))
            (lyrics-url (if hits
                            (gethash 'path
                                     (gethash 'result
                                              (car hits))))))
       (funcall callback lyrics-url)))))

(provide 'spotify-lyrics)
;;; spotify-lyrics.el ends here
