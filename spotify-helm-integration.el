;;; package --- Summary

;;; Commentary:

;; spotify-helm-integration.el --- Spotify.el helm integration

;; Code:


(defvar helm-playlists-doc-header
  " (\\<helm-playlists-map>\\[helm-playlists-load-more-interactive]: Load more playlists)"
  "*The doc that is inserted in the Name header of the helm spotify source.")

(defvar helm-tracks-doc-header
  " (\\<helm-tracks-map>\\[helm-tracks-load-more-interactive]: Load more tracks)"
  "*The doc that is inserted in the Name header of the helm spotify source.")


;; Helm-Keymaps

(defvar helm-playlists-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-l") 'helm-playlists-load-more-interactive)
    (define-key map (kbd "C-M-f") 'helm-playlists-follow-interactive)
    (define-key map (kbd "C-M-u") 'helm-playlists-unfollow-interactive)
    map)
  "Local keymap for playlists in helm buffers")

(defvar helm-tracks-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-q") 'helm-tracks-enqueue-interactive)
    (define-key map (kbd "C-l") 'helm-tracks-load-more-interactive)
    (define-key map (kbd "C-M-a") 'helm-tracks-view-album-interactive)
    map)
  "Local keymap for tracks in helm buffers")


;;; Helm-playlists
;;
;;

(defcustom helm-playlists-actions (helm-make-actions
                                   "View playlist's tracks `RET'" 'helm-playlists-view-tracks-core
                                   "Load more playlists `C-l'" 'helm-playlists-load-more-core
                                   "Follow playlist `C-M-f'" 'helm-playlists-follow-core
                                   "Unfollow playlist `C-M-u'" 'helm-playlists-unfollow-core)
  "Actions for playlists in helm buffers"
  :group 'spotify
  :type '(alist :key-type string :value-type function))

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

(defun helm-playlists (source-name)
  "This will use the tab buffer generated from loading playlist items as a source for helm to 
operate on"  
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


;;; Helm-tracks
;;
;;

(defcustom helm-tracks-actions (helm-make-actions
                                "Play track `RET'" 'helm-tracks-select-default-core
                                "Enqueue track `C-q'" 'helm-tracks-enqueue-core
                                "Load more tracks `C-l'" 'helm-tracks-load-more-core
                                "View album of track `C-M-a'" 'helm-tracks-view-album-core)
  "Actions for tracks in helm buffers"
  :group 'spotify
  :type '(alist :key-type string :value-type function))

(defun helm-tracks-select-default-core (candidate)
  "Helm action to play a selected track & clean up dangling Spotify buffers"
  (spotify-track-select-default candidate)
  (unless helm-in-persistent-action
    (helm-spotify-cleanup-buffers)))

(defun helm-tracks-load-more-interactive ()
  "Helm action wrapper to bind to a key map"
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-tracks-load-more-core)))

(defun helm-tracks-load-more-core (_candidate)
  "Helm action to load more tracks"
  (spotify-track-load-more))

(defun helm-tracks-view-album-interactive ()
  "Helm action wrapper to bind to a key map"
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-tracks-view-album-core)))

(defun helm-tracks-view-album-core (candidate)
  "Helm action to view a track's album context"
  (let ((album (spotify-get-track-album candidate)))
    (spotify-album-tracks album)))

(defun helm-tracks-enqueue-interactive ()
  "Helm action wrapper to bind to a key map"
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'enqueue '(helm-tracks-enqueue-core . never-split))
    (helm-execute-persistent-action 'enqueue)))

(defun helm-tracks-enqueue-core (candidate)
  "Helm action to enqueue a track into the active device's playback"
  (hash-table-keys candidate)
  (lexical-let ((name (spotify-get-item-name candidate))
                (uri (spotify-get-item-uri candidate)))
    (spotify-api-enqueue uri (lambda (_)
                               (message (format "Added '%s' to playback queue" name))))))

(defun helm-tracks (source-name)
  "This will use the tab buffer generated from loading track items as a source for helm to 
operate on"
  (lexical-let ((tabulated-list-entries tabulated-list-entries))
    (helm :sources (helm-build-in-buffer-source source-name
                     :header-name (lambda (name)
                                    (concat name (substitute-command-keys helm-tracks-doc-header)))
                     :data (current-buffer)
                     :get-line #'buffer-substring
                     :display-to-real (lambda (_candidate)
                                        (let* ((candidate
                                                (helm-get-selection nil 'withprop))
                                               (tabulated-list-id
                                                (get-text-property 0 'tabulated-list-id candidate)))
                                          tabulated-list-id))
                     :action helm-tracks-actions
                     :keymap helm-tracks-map
                     :fuzzy-match t)
          :buffer "*helm spotify*")))


;;; Helm-devices
;;
;;

(defcustom helm-devices-actions (helm-make-actions
                                "Select device `RET'" 'helm-devices-select-core)
  "Actions for devices in helm buffers"
  :group 'spotify
  :type '(alist :key-type string :value-type function))

(defun helm-devices-select-core (candidate)
  "Helm action to make the selected device active"
  (lexical-let ((device-id (spotify-get-device-id candidate))
                (name (spotify-get-device-name candidate)))
    (spotify-api-transfer-player
     device-id
     (lambda (json)
       (setq spotify-selected-device-id device-id)
       (message "Device '%s' selected" name)))
    (helm-spotify-cleanup-buffers)))

(defun helm-devices (source-name)
  "This will use the tab buffer generated from loading device items as a source for helm to 
operate on"  
  (lexical-let ((tabulated-list-entries tabulated-list-entries))
    (helm :sources (helm-build-in-buffer-source source-name
                     :data (current-buffer)
                     :get-line #'buffer-substring
                     :display-to-real (lambda (_candidate)
                                        (let* ((candidate
                                                (helm-get-selection nil 'withprop))
                                               (tabulated-list-id
                                                (get-text-property 0 'tabulated-list-id candidate)))
                                          tabulated-list-id))
                     :action helm-devices-actions
                     :fuzzy-match t)
          :buffer "*helm spotify*")))


;;; Misc
;;
;;

(defun helm-spotify-cleanup-buffers ()
  "Cleanup dangling tabulated-mode buffers from the core search APIs."
  (let ((buffer-list (mapcar (lambda (buffer) (buffer-name buffer)) (buffer-list)))
        (spotify-buffer-candidates '("*Devices*"
                                     "*Featured Playlists*"
                                     "*Recently Played*"
                                     "\*Playlists: .*\*"
                                     "\*Playlist Search: .*\*"
                                     "\*Track Search: .*\*"
                                     "\*Playlist Tracks: .*\*"
                                     "\*Album: .*\*")))
    (mapc (lambda (spotify-buffer) (kill-buffer spotify-buffer))
          (seq-filter (lambda (buffer)
                        (when (some (lambda (candidate) (string-match-p candidate buffer))
                                    spotify-buffer-candidates)
                          buffer))
                      buffer-list))))

(provide 'spotify-helm-integration)
