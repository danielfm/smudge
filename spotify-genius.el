;;; package --- Summary

;;; Commentary:

;; spotify-genius.el --- Spotify.el interface for Genius lyrics

;; Copyright (C) 2019 Jason Dufair

;;; Code:

(require 'json)

(defcustom spotify-genius-token ""
  "The OAuth token generated at genius.com."
  :group 'spotify
  :type 'string)

(defconst spotify-genius-endpoint "https://api.genius.com")
(defconst spotify-genius-website-url "https://genius.com")

(defun genius-api-call-async (method uri &optional data callback is-retry)
  "Make a request to the genius API endpoint via METHOD with provided URI and optional DATA.  Call CALLBACK with the parsed JSON response.  Only retry if not IS-RETRY."
  (lexical-let ((method method)
                (uri uri)
                (data data)
                (callback callback)
                (is-retry is-retry))
    (oauth2-url-retrieve
     (make-oauth2-token :access-token spotify-genius-token)
     (concat spotify-genius-endpoint uri)
     (lambda (_)
       (toggle-enable-multibyte-characters t)
       (goto-char (point-min))
       (condition-case err
           (when (search-forward-regexp "^$" nil t)
             (let* ((json-object-type 'hash-table)
                    (json-array-type 'list)
                    (json-key-type 'symbol)
                    (json (json-read))
                    (error-json (gethash 'error json)))
               (kill-buffer)

               ;; Retries the request when the token expires and gets refreshed
               (if (and (hash-table-p error-json)
                        (eq 401 (gethash 'status error-json))
                        (not is-retry))
                   (genius-api-call-async method uri data callback t)
                 (when callback (funcall callback json)))))

         ;; Handle empty responses
         (end-of-file
          (kill-buffer)
          (when callback (funcall callback nil)))))
     nil
     method
     (or data "")
     '(("Content-Type" . "application/json")))))

(defun spotify-genius-search (artist title callback)
  "Search the genius API by ARTIST and TITLE.  Call CALLBACK with the URL of the lyrics."
  (lexical-let ((artist artist)
                (title title)
                (callback callback))
    (genius-api-call-async
     "GET"
     (concat "/search?"
             (url-build-query-string
              `((q      ,(concat artist title)))
              nil t))
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
       (let ((parsed-html (libxml-parse-html-region (point) (point-max))))
         (kill-buffer)
         (funcall callback parsed-html))))))

;; (spotify-genius-search
;;  "Aesop Rock" "Shrunk"
;;  (lambda (lyrics-url)
;;    (spotify-genius-get-lyrics
;;     lyrics-url
;;     (lambda (result)
;;       (setq foobar result)
;;       (message "done")))))

;;(shr-insert-document foobar)



;;; spotify-genius.el ends here
