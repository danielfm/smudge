;; spotify-remote.el --- Spotify.el remote minor mode

;; Copyright (C) 2014-2018 Daniel Fernandes Martins

;;; Code:

(require 'easymenu)


(defvar spotify-player-status ""
  "The text to be displayed in the global mode line or title bar.")
(defcustom spotify-status-location 'modeline
  "Specify where to show the player status: one of '(modeline title-bar nil)."
  :type '(choice (const :tag "Modeline" modeline)
                (const :tag "Title Bar" title-bar)
                (const :tag "Do not show" nil))
  :group 'spotify)

(define-minor-mode spotify-remote-mode
  "Toggles Spotify Remote mode.
A positive prefix argument enables the mode, any other prefix
argument disables it. From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When Spotify Remote mode is enabled, it's possible to toggle
the repeating and shuffling status of the running Spotify process.
See commands \\[spotify-toggle-repeating] and
\\[spotify-toggle-shuffling]."
  :group 'spotify
  :init-value nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-p M-r") 'spotify-toggle-repeat)
            (define-key map (kbd "M-p M-s") 'spotify-toggle-shuffle)
            (define-key map (kbd "M-p M-p") 'spotify-toggle-play)
            (define-key map (kbd "M-p M-b") 'spotify-previous-track)
            (define-key map (kbd "M-p M-f") 'spotify-next-track)
            (define-key map (kbd "M-p p m") 'spotify-my-playlists)
            (define-key map (kbd "M-p p f") 'spotify-featured-playlists)
            (define-key map (kbd "M-p p u") 'spotify-user-playlists)
            (define-key map (kbd "M-p p s") 'spotify-playlist-search)
            (define-key map (kbd "M-p p c") 'spotify-create-playlist)
            (define-key map (kbd "M-p t s") 'spotify-track-search)
            (define-key map (kbd "M-p v u") 'spotify-volume-up)
            (define-key map (kbd "M-p v d") 'spotify-volume-down)
						(define-key map (kbd "M-p M-d") 'spotify-select-device)
            map)
  (let ((s '(:eval (spotify-player-status-text))))
    (if spotify-remote-mode
        (progn
          (spotify-start-player-status-timer)
          (cond ((eq spotify-status-location 'modeline)
                 (unless (member s global-mode-string)
                   (push s global-mode-string)))
                ((eq spotify-status-location 'title-bar)
                 ;; title-bar-format may be just a string.
                 ;; make it a list so we can push player status on to it
                 (if (stringp frame-title-format)
                     (setq frame-title-format (list frame-title-format)))
                 (unless (member s frame-title-format)
                   (nconc frame-title-format `("    " ,s))))))
      (progn
        (spotify-stop-player-status-timer)
        (cond ((eq spotify-status-location 'modeline)
               (when (member s global-mode-string)
                 (setq global-mode-string (remove s global-mode-string))))
              ((eq spotify-status-location 'title-bar)
               (when (member s frame-title-format)
                 (setq frame-title-format
                       (remove "    " (remove s frame-title-format))))))))))

(easy-menu-add-item nil '("Tools")
  '("Spotify"
    ["Play/Pause"     spotify-toggle-play    :active spotify-remote-mode]
    ["Previous Track" spotify-previous-track :active spotify-remote-mode]
    ["Next Track"     spotify-next-track     :active spotify-remote-mode]
    "--"
    ["Shuffle" spotify-toggle-shuffle :style toggle :active (and spotify-remote-mode (spotify-is-shuffling-supported)) :selected (spotify-is-shuffling)]
    ["Repeat"  spotify-toggle-repeat  :style toggle :active (and spotify-remote-mode (spotify-is-repeating-supported)) :selected (spotify-is-repeating)]
    "--"
    ["Search Tracks..."    spotify-track-search       :active (spotify-remote-mode)]
    ["Featured Playlists"  spotify-featured-playlists :active (spotify-remote-mode)]
    ["My Playlists"        spotify-my-playlists       :active (spotify-remote-mode)]
    ["User Playlists..."   spotify-user-playlists     :active (spotify-remote-mode)]
    ["Search Playlists..." spotify-playlist-search    :active (spotify-remote-mode)]
    ["Create Playlist..."  spotify-create-playlist    :active (spotify-remote-mode)]
    "--"
    ["Spotify Remote Mode" spotify-remote-mode :style toggle :selected spotify-remote-mode]))

(defun spotify-remote-popup-menu ()
  (interactive)
  (popup-menu
   '("Spotify"
     ["Play/Pause" spotify-toggle-play :active t]
     ["Previous Track" spotify-previous-track :active t]
     ["Next Track" spotify-next-track :active t]
     "--"
     ["Shuffle" spotify-toggle-shuffle :style toggle :active (spotify-is-shuffling-supported) :selected (spotify-is-shuffling)]
     ["Repeat"  spotify-toggle-repeat  :style toggle :active (spotify-is-repeating-supported) :selected (spotify-is-repeating)]
     "--"
     ["Search Tracks..."    spotify-track-search       :active t]
     ["Featured Playlists"  spotify-featured-playlists :active t]
     ["My Playlists"        spotify-my-playlists       :active t]
     ["User Playlists..."   spotify-user-playlists     :active t]
     ["Search Playlists..." spotify-playlist-search    :active t]
     ["Create Playlist..."  spotify-create-playlist    :active t])))

(defvar spotify-remote-player-status-map (make-sparse-keymap))
(define-key spotify-remote-player-status-map [player-status mouse-1] 'spotify-remote-popup-menu)

(defun spotify-update-player-status (str)
  "Set the given STR to the player status, prefixed with the mode identifier."
  (when (not (string= str spotify-player-status))
    (setq spotify-player-status str)))

(defun spotify-player-status-text ()
  "Return the propertized text to be displayed as the lighter."
  (propertize spotify-player-status
              'local-map spotify-remote-player-status-map
              'mouse-face 'player-status-highlight))

(defun turn-on-spotify-remote-mode ()
  "Turn the `spotify-remote-mode' on in the current buffer."
  (spotify-remote-mode 1))


(define-globalized-minor-mode global-spotify-remote-mode
  spotify-remote-mode turn-on-spotify-remote-mode
  :group 'spotify)

(provide 'spotify-remote)
