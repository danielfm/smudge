;;; smudge-image.el --- Smudge image support library  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Jason Dufair

;;; Commentary:

;; This library implements methods that support image display for smudge

;;; Code:

(defcustom smudge-show-artwork t
	"Whether to show artwork when searching for tracks."
	:type 'boolean
	:group 'smudge)

(defvar smudge-artwork-fetch-target-count 0)
(defvar smudge-artwork-fetch-count 0)

(defun smudge-image-increment-count ()
	"Increment count of fetched (or the absence of) images.  Handle redisplay."
	(setq smudge-artwork-fetch-count (1+ smudge-artwork-fetch-count))
	(when (= smudge-artwork-fetch-count smudge-artwork-fetch-target-count)
		(setq inhibit-redisplay nil)))

(defun smudge-image-tabulated-list-print-entry (id cols)
  "Insert a Tabulated List entry at point.
This implementation asynchronously inserts album images in the
table buffer after the rows are printed.  It reimplements most of
the `tabulated-list-print-entry' function but depends on a url
being the first column's data.  It does not print that url in the
column.  ID is a Lisp object identifying the entry to print, and
COLS is a vector of column descriptors."
  (let ((beg   (point))
				 (x     (max tabulated-list-padding 0))
				 (ncols (length tabulated-list-format))
				 (inhibit-read-only t)
				 (cb (current-buffer))
				 (image-url (aref cols 0)))
    (if (> tabulated-list-padding 0)
			(insert (make-string x ?\s)))
		(if image-url
			(url-retrieve image-url
				(lambda (_)
					(let ((img (create-image
											 (progn
												 (goto-char (point-min))
												 (re-search-forward "^$")
												 (forward-char)
												 (delete-region (point) (point-min))
												 (buffer-substring-no-properties (point-min) (point-max)))
											 nil t :width 64)))
						;; kill the image data buffer. We have the data now
						(kill-buffer)
						;; switch to the table buffer
						(set-buffer cb)
						(let ((inhibit-read-only t))
							(save-excursion
								(goto-char beg)
								(put-image img (point) "track image" 'left-margin)))
						(smudge-image-increment-count))))
			(smudge-image-increment-count))
		(insert ?\s)
    (let ((tabulated-list--near-rows ; Bind it if not bound yet (Bug#25506).
						(or (bound-and-true-p tabulated-list--near-rows)
              (list (or (tabulated-list-get-entry (point-at-bol 0))
                      cols)
                cols))))
			;; don't print the URL column
      (dotimes (n (- ncols 1))
        (setq x (tabulated-list-print-col (+ n 1) (aref cols (+ n 1)) x))))
    (insert ?\n)
		;; Ever so slightly faster than calling `put-text-property' twice.
    (add-text-properties
			beg (point)
			`(tabulated-list-id ,id tabulated-list-entry ,cols))))

(provide 'smudge-image)
;;; smudge-image.el ends here
