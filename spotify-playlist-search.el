;; spotify-playlist-search.el --- Spotify.el playlist search major mode

;; Copyright (C) 2014-2018 Daniel Fernandes Martins

;; Code:

(require 'spotify-api)

(defvar helm-playlists-doc-header
  " (\\<helm-playlists-map>\\[helm-playlists-load-more-interactive]: Load more playlists)"
  "*The doc that is inserted in the Name header of the helm spotify source.")

(defun helm-playlists-view-tracks-core (candidate)
  "Helm action to view all tracks of the selected playlist"
  (when helm-in-persistent-action
    (helm-exit-minibuffer))
  (spotify-playlist-tracks candidate))

(defun helm-playlists-load-more-interactive ()
  "Helm action wrapper to bind to a key map"
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-playlists-load-more-core)))

(defun helm-playlists-load-more-core (_candidate)
  "Helm action to load more playlists"
  (spotify-playlist-load-more))

(defun helm-playlists-follow-interactive ()
  "Helm action wrapper to bind to a key map"
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'follow '(helm-playlists-follow-core . never-split))
    (helm-execute-persistent-action 'follow)))

(defun helm-playlists-follow-core (candidate)
  "Helm action to follow the selected playlist"
  (spotify-playlist-follow candidate))

(defun helm-playlists-unfollow-interactive ()
  "Helm action wrapper to bind to a key map"
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'unfollow '(helm-playlists-unfollow-core . never-split))
    (helm-execute-persistent-action 'unfollow)))

(defun helm-playlists-unfollow-core (candidate)
  "Helm action to unfollow the selected playlist"
  (spotify-playlist-unfollow candidate))

(defcustom helm-playlists-actions (helm-make-actions
                                   "View playlist's tracks `RET'" 'helm-playlists-view-tracks-core
                                   "Load more playlists `C-l'" 'helm-playlists-load-more-core
                                   "Follow playlist `C-M-f'" 'helm-playlists-follow-core
                                   "Unfollow playlist `C-M-u'" 'helm-playlists-unfollow-core)
  "Actions for playlists in helm buffers"
  :group 'spotify
  :type '(alist :key-type string :value-type function))

(defvar helm-playlists-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-l") 'helm-playlists-load-more-interactive)
    (define-key map (kbd "C-M-f") 'helm-playlists-follow-interactive)
    (define-key map (kbd "C-M-u") 'helm-playlists-unfollow-interactive)
    map)
  "Local keymap for playlists in helm buffers")

(defvar spotify-playlist-search-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "M-RET") 'spotify-playlist-select)
    (define-key map (kbd "l")     'spotify-playlist-load-more)
    (define-key map (kbd "g")     'spotify-playlist-reload)
    (define-key map (kbd "f")     'spotify-playlist-follow)
    (define-key map (kbd "u")     'spotify-playlist-unfollow)
    map)
  "Local keymap for `spotify-playlist-search-mode' buffers.")

;; Enables the `spotify-remote-mode' the track search buffer
(add-hook 'spotify-playlist-search-mode-hook 'spotify-remote-mode)

(define-derived-mode spotify-playlist-search-mode tabulated-list-mode "Playlist-Search"
  "Major mode for displaying the playlists returned by a Spotify search.")

(defun spotify-playlist-select ()
  "Plays the playlist under the cursor."
  (interactive)
  (let ((selected-playlist (tabulated-list-get-id)))
    (spotify-play-track nil selected-playlist)))

(defun spotify-playlist-reload ()
  "Reloads the first page of results for the current playlist view."
  (interactive)
  (let ((page 1))
    (cond ((bound-and-true-p spotify-query)          (spotify-playlist-search-update spotify-query page))
          ((bound-and-true-p spotify-browse-message) (spotify-featured-playlists-update page))
          (t                                         (spotify-user-playlists-update spotify-user-id page)))))

(defun spotify-playlist-load-more ()
  "Loads the next page of results for the current playlist view."
  (interactive)
  (let ((next-page (1+ spotify-current-page)))
    (cond ((bound-and-true-p spotify-query)          (spotify-playlist-search-update spotify-query next-page))
          ((bound-and-true-p spotify-browse-message) (spotify-featured-playlists-update next-page))
          (t (if (and spotify-helm-integration (eq playlists-loaded total-playlists))
                 (spotify-user-playlists-update spotify-user-id spotify-current-page)
               (spotify-user-playlists-update spotify-user-id next-page))))))

(defun spotify-playlist-follow (&optional helm-selection-id)
  "Adds the current user as the follower of the playlist under the cursor."
  (interactive)
  (lexical-let* ((selected-playlist (or helm-selection-id (tabulated-list-get-id)))
                 (name (spotify-get-item-name selected-playlist)))
    (when (y-or-n-p (format "Follow playlist '%s'?" name))
      (spotify-api-playlist-follow
       selected-playlist
       (lambda (_)
         (message (format "Followed playlist '%s'" name)))))))

(defun spotify-playlist-unfollow (&optional helm-selection-id)
  "Removes the current user as the follower of the playlist under the cursor."
  (interactive)
  (lexical-let* ((selected-playlist (or helm-selection-id (tabulated-list-get-id)))
                 (name (spotify-get-item-name selected-playlist)))
    (when (y-or-n-p (format "Unfollow playlist '%s'?" name))
      (spotify-api-playlist-unfollow
       selected-playlist
       (lambda (_)
         (message (format "Unfollowed playlist '%s'" name)))))))

(defun helm-source-playlists-from-current-buffer (source-name)
  "Available only if helm integration is enabled & helm is installed
This will use the tab buffer generated as a source for helm to operate on"
  (lexical-let ((tabulated-list-entries tabulated-list-entries))
    (helm :sources (helm-build-in-buffer-source source-name
                     :header-name (lambda (name)
                         (concat name (substitute-command-keys helm-playlists-doc-header)))
                     :data (current-buffer)
                     :get-line #'buffer-substring
                     :display-to-real (lambda (_candidate)
                                        (let* ((candidate
                                                (helm-get-selection nil 'withprop))
                                               (tabulated-list-id
                                                (get-text-property 0 'tabulated-list-id candidate)))
                                          tabulated-list-id))
                     :action helm-playlists-actions
                     :keymap helm-playlists-map
                     :fuzzy-match t)
          :buffer "*helm spotify*")))

(defun spotify-playlist-search-update (query current-page)
  "Fetches the given page of results using the search endpoint."
  (lexical-let ((current-page current-page)
                (query query)
                (buffer (current-buffer)))
    (spotify-api-search
     'playlist
     query
     current-page
     (lambda (playlists)
       (if-let ((items (spotify-get-search-playlist-items playlists)))
           (with-current-buffer buffer
             (setq-local spotify-current-page current-page)
             (setq-local spotify-query query)
             (if (and spotify-helm-integration (package-installed-p 'helm))
                 (progn
                   (spotify-playlist-search-print items current-page)
                   (helm-source-playlists-from-current-buffer
                    (format "Spotify Playlists - Search Results for \"%s\"" spotify-query)))
               (pop-to-buffer buffer)
               (spotify-playlist-search-print items current-page)
               (message "Playlist view updated")))
         (message "No more playlists"))))))

(defun spotify-user-playlists-update (user-id current-page)
  "Fetches the given page of results using the user's playlist endpoint."
  (lexical-let ((user-id user-id)
                (current-page current-page)
                (buffer (current-buffer)))
    (spotify-api-user-playlists
     user-id
     current-page
     (lambda (playlists)
       (if-let ((items (spotify-get-items playlists)))
           (with-current-buffer buffer
             (setq-local spotify-user-id user-id)
             (setq-local spotify-current-page current-page)
             (if (and spotify-helm-integration (package-installed-p 'helm))
                 (progn
                   (setq-local current-page-size (length (gethash 'items json)))
                   (setq-local playlists-loaded (+ (* 50 (1- current-page)) current-page-size))
                   (setq-local total-playlists (gethash 'total json))
                   (spotify-playlist-search-print items current-page)
                   (helm-source-playlists-from-current-buffer
                    (format "Spotify Playlists - %s (%s/%s playlists loaded)"
                            spotify-user-id
                            playlists-loaded
                            total-playlists)))
               (pop-to-buffer buffer)
               (spotify-playlist-search-print items current-page)
               (message "Playlist view updated")))
         (message "No more playlists"))))))

(defun spotify-featured-playlists-update (current-page)
  "Fetches the given page of results using of Spotify's featured playlists"
  (lexical-let ((current-page current-page)
                (buffer (current-buffer)))
    (spotify-api-featured-playlists
     current-page
     (lambda (json)
       (if-let ((items (spotify-get-search-playlist-items json))
                (msg (spotify-get-message json)))
           (with-current-buffer buffer
             (setq-local spotify-current-page current-page)
             (setq-local spotify-browse-message msg)
             (if (and spotify-helm-integration (package-installed-p 'helm))
                 (progn
                   (spotify-playlist-search-print items current-page)
                   (helm-source-playlists-from-current-buffer "Spotify Playlists - Featured"))
               (pop-to-buffer buffer)
               (spotify-playlist-search-print items current-page)
               (message "Playlist view updated")))
         (message "No more playlists"))))))

(defun spotify-playlist-tracks (&optional helm-selection-id)
  "Displays the tracks that belongs to the playlist by either the value under the cursor or
from the selection in helm."
  (interactive)
  (let* ((selected-playlist (or helm-selection-id (tabulated-list-get-id)))
         (name (spotify-get-item-name selected-playlist))
         (buffer (get-buffer-create (format "*Playlist Tracks: %s*" name))))
    (with-current-buffer buffer
      (spotify-track-search-mode)
      (setq-local spotify-selected-playlist selected-playlist)
      (spotify-playlist-tracks-update 1))))

(defun spotify-playlist-set-list-format ()
  "Configures the column data for the typical playlist view."
  (setq tabulated-list-format
        (vector `("Playlist Name" ,(- (window-width) 45) t)
                '("Owner Id" 30 t)
                '("# Tracks" 8 (lambda (row-1 row-2)
                                 (< (spotify-get-playlist-track-count (first row-1))
                                    (spotify-get-playlist-track-count (first row-2)))) :right-align t))))

(defun spotify-playlist-search-print (playlists current-page)
  "Appends the given playlists to the current playlist view."
  (let (entries)
    (dolist (playlist playlists)
      (let ((user-id (spotify-get-playlist-owner-id playlist))
            (playlist-name (spotify-get-item-name playlist)))
        (push (list playlist
                    (vector (cons playlist-name
                                  (list 'face 'link
                                        'follow-link t
                                        'action `(lambda (_) (spotify-playlist-tracks))
                                        'help-echo (format "Show %s's tracks" playlist-name)))
                            (cons user-id
                                  (list 'face 'link
                                        'follow-link t
                                        'action `(lambda (_) (spotify-user-playlists ,user-id))
                                        'help-echo (format "Show %s's public playlists" user-id)))
                            (number-to-string (spotify-get-playlist-track-count playlist))))
              entries)))
    (when (eq 1 current-page) (setq-local tabulated-list-entries nil))
    (spotify-playlist-set-list-format)
    (setq-local tabulated-list-entries (append tabulated-list-entries (nreverse entries)))
    (tabulated-list-init-header)
    (tabulated-list-print t)))


(provide 'spotify-playlist-search)
