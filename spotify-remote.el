;; spotify-remote.el --- Spotify.el remote minor mode

;; Copyright (C) 2014-2019 Daniel Fernandes Martins

;; Code:

(require 'easymenu)

;; This variable keeps the text to be displayed in the global mode line
(defvar spotify-mode-line "")

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
            (define-key map (kbd "M-p t r") 'spotify-recently-played)
            (define-key map (kbd "M-p v u") 'spotify-volume-up)
            (define-key map (kbd "M-p v d") 'spotify-volume-down)
            (define-key map (kbd "M-p v m") 'spotify-volume-mute-unmute)
            (define-key map (kbd "M-p d") 'spotify-select-device)
            map)
  (let ((s '(:eval (spotify-mode-line-text))))
    (if spotify-remote-mode
        (progn
          (spotify-start-mode-line-timer)
          (unless (member s global-mode-string)
            (push s global-mode-string)))
      (progn
        (spotify-stop-mode-line-timer)
        (when (member s global-mode-string)
          (setq global-mode-string (remq s global-mode-string)))))))

(easy-menu-add-item nil '("Tools")
  '("Spotify"
    ["Play/Pause"     spotify-toggle-play    :active spotify-remote-mode]
    ["Previous Track" spotify-previous-track :active spotify-remote-mode]
    ["Next Track"     spotify-next-track     :active spotify-remote-mode]
    "--"
    ["Shuffle" spotify-toggle-shuffle :active spotify-remote-mode]
    ["Repeat"  spotify-toggle-repeat  :active spotify-remote-mode]
    "--"
    ["Search Tracks..."    spotify-track-search       :active spotify-remote-mode]
    ["Featured Playlists"  spotify-featured-playlists :active spotify-remote-mode]
    ["My Playlists"        spotify-my-playlists       :active spotify-remote-mode]
    ["User Playlists..."   spotify-user-playlists     :active spotify-remote-mode]
    ["Search Playlists..." spotify-playlist-search    :active spotify-remote-mode]
    ["Create Playlist..."  spotify-create-playlist    :active spotify-remote-mode]
    "--"
    ["Spotify Remote Mode" spotify-remote-mode :style toggle :selected spotify-remote-mode]))

(defun spotify-remote-popup-menu ()
  (interactive)
  (popup-menu
   '("Spotify"
     ["Play/Pause" spotify-toggle-play]
     ["Previous Track" spotify-previous-track]
     ["Next Track" spotify-next-track]
     "--"
     ["Volume Up" spotify-volume-up]
     ["Volume Down" spotify-volume-down]
     ["Mute/Unmute" spotify-volume-mute-unmute]
     "--"
     ["Shuffle" spotify-toggle-shuffle]
     ["Repeat"  spotify-toggle-repeat]
     "--"
     ["Search Tracks..."    spotify-track-search]
     ["Featured Playlists"  spotify-featured-playlists]
     ["My Playlists"        spotify-my-playlists]
     ["User Playlists..."   spotify-user-playlists]
     ["Search Playlists..." spotify-playlist-search]
     ["Create Playlist..."  spotify-create-playlist])))

(defvar spotify-remote-mode-line-map (make-sparse-keymap))
(define-key spotify-remote-mode-line-map [mode-line mouse-1] 'spotify-remote-popup-menu)

(defun spotify-update-mode-line (str)
  "Sets the given str to the mode line, prefixed with the mode identifier."
  (when (not (string= str spotify-mode-line))
    (setq spotify-mode-line str)
    (force-mode-line-update t)))

(defun spotify-mode-line-text ()
  "Returns the propertized text to be displayed as the lighter."
  (propertize spotify-mode-line
              'local-map spotify-remote-mode-line-map
              'mouse-face 'mode-line-highlight))

(defun turn-on-spotify-remote-mode ()
  "Turns the `spotify-remote-mode' on in the current buffer."
  (spotify-remote-mode 1))


(define-globalized-minor-mode global-spotify-remote-mode
  spotify-remote-mode turn-on-spotify-remote-mode
  :group 'spotify)

(provide 'spotify-remote)
