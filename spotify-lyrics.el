;;; package --- Summary

;;; Commentary:

;; spotify-lyrics.el --- Spotify.el interface for Genius lyrics

;; Copyright (C) 2019 Jason Dufair

;;; Code:

(require 'json)

(defcustom spotify-genius-token ""
  "The OAuth token generated at genius.com."
  :group 'spotify
  :type 'string)

(defconst spotify-genius-endpoint "https://api.genius.com")
(defconst spotify-genius-website-url "https://genius.com")

(defun spotify-genius-search (metadata callback)
  "Search the genius API using METADATA.  Call CALLBACK with the URL of the lyrics."
  (lexical-let ((metadata metadata)
                (callback callback))
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
         (funcall callback lyrics-url))))))

(defun spotify-genius-get-lyrics (uri callback)
  "Get lyrics from URI.  Parse the resultant HTML to strip out the lyrics into plain text.  Call CALLBACK with plain text lyrics."
  (lexical-let ((uri uri)
                (callback callback))
    (url-retrieve
     (concat spotify-genius-website-url uri)
     (lambda (_)
       (toggle-enable-multibyte-characters t)
       (goto-char (point-min))
       (search-forward-regexp "<!DOCTYPE html>")
       (forward-char)
       (let ((parsed-html (libxml-parse-html-region (point) (point-max) spotify-genius-website-url)))
         (kill-buffer)
         (funcall callback parsed-html))))))

(provide 'spotify-lyrics)
;;; spotify-lyrics.el ends here
