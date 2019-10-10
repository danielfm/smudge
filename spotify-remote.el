;; spotify-remote.el --- Spotify.el remote minor mode

;; Copyright (C) 2014-2019 Daniel Fernandes Martins

;;; Code:

(require 'easymenu)

(defcustom spotify-keymap-prefix nil
  "Spotify remote keymap prefix."
  :group 'spotify
  :type 'string)

(defcustom spotify-status-location 'modeline
  "Specify where to show the player status: one of '(modeline title-bar nil)."
  :type '(choice (const :tag "Modeline" modeline)
                 (const :tag "Title Bar" title-bar)
                 (const :tag "Do not show" nil))
  :group 'spotify)

(defcustom spotify-title-bar-separator "    "
  "The string used to separate the player status from the remaining text on the title bar."
  :type 'string
  :group 'spotify)

(defvar spotify-player-status ""
  "The text to be displayed in the global mode line or title bar.")

(defvar spotify-command-map
  (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-r") #'spotify-toggle-repeat)
            (define-key map (kbd "M-s") #'spotify-toggle-shuffle)
            (define-key map (kbd "M-p") #'spotify-toggle-play)
            (define-key map (kbd "M-b") #'spotify-previous-track)
            (define-key map (kbd "M-u") #'spotify-volume-up)
            (define-key map (kbd "M-d") #'spotify-volume-down)
            (define-key map (kbd "M-f") #'spotify-next-track)
            (define-key map (kbd "p m") #'spotify-my-playlists)
            (define-key map (kbd "p f") #'spotify-featured-playlists)
            (define-key map (kbd "p u") #'spotify-user-playlists)
            (define-key map (kbd "p s") #'spotify-playlist-search)
            (define-key map (kbd "p c") #'spotify-create-playlist)
            (define-key map (kbd "t s") #'spotify-track-search)
            (define-key map (kbd "d") #'spotify-select-device)
            map)
  "Keymap for Spotify commands after 'spotify-keymap-prefix'.")
(fset 'spotify-command-map spotify-command-map)

(defvar spotify-mode-map
  (let ((map (make-sparse-keymap)))
    (when spotify-keymap-prefix
      (define-key map spotify-keymap-prefix 'spotify-command-map))
    map)
  "Keymap for Spotify remote mode.")

(defun spotify-add-frame-title-status (status title-section)
  "Return the TITLE-SECTION or part thereof with the player STATUS appended."
  (if (stringp title-section)
      (cons title-section (list status))
    (append title-section (list status))))

(defun spotify-set-frame-title (status)
  "Parse and set the frame title, appending STATUS to all frame scenarios."
  (spotify-remove-status-from-frame-title status)
  (setq frame-title-format
        (if (and (listp frame-title-format) (eq (car frame-title-format) 'multiple-frames))
            (let ((multiple-frame-format (car (nthcdr 1 frame-title-format)))
                      (single-frame-format (car (nthcdr 2 frame-title-format))))
                  (list 'multiple-frames
                        (spotify-add-frame-title-status status multiple-frame-format)
                        (spotify-add-frame-title-status status single-frame-format)))
          (spotify-add-frame-title-status status frame-title-format))))

(defun spotify-remove-status-from-frame-title (status)
  "Parse the frame title and remove the player STATUS."
  (setq frame-title-format
        (if (not (listp frame-title-format))
            frame-title-format
          (if (member status frame-title-format)
            (remove status frame-title-format)
          (mapcar (lambda (section)
                    (if (and (listp section) (member status section))
                        (remove status section)
                      section))
                  frame-title-format)))))

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
  :keymap spotify-mode-map
  (let ((s `(,spotify-title-bar-separator (:eval (spotify-player-status-text)))))
    (if spotify-remote-mode
        (progn
          (spotify-start-player-status-timer)
          (cond ((or (eq spotify-status-location 'modeline) (not (display-graphic-p)))
                 (unless (member s global-mode-string)
                   (push s global-mode-string)))
                ((eq spotify-status-location 'title-bar)
                 (spotify-set-frame-title s))))
      (progn
        (spotify-stop-player-status-timer)
        (cond ((or (eq spotify-status-location 'modeline) (not (display-graphic-p)))
               (when (member s global-mode-string)
                 (setq global-mode-string (remove s global-mode-string))))
              ((eq spotify-status-location 'title-bar)
               (spotify-remove-status-from-frame-title s)))))))

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
