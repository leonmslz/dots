;; init.el
;; Configuration File For GNU Emacs
;; By Leon Schulz

;; :==:> Package Management

;; => MELPA Repositories
;; Milkypostman’s Emacs Lisp Package Archive <https://melpa.org/>

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; => Use-Package
;; Macro For Simplyfiying And Isolating Package Configurations <https://github.com/jwiegley/use-package>

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; :==:> General Settings

;; => Emacs Startup Time
;; Display Emacs Startup Time On Startup :D <https://github.com/daviwil/dotfiles>

;; The Default Is 800 Kilobytes. Measured In Bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile Emacs Startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

;; => Encodings

;; UTF-8 As Default Encoding
(set-language-environment    'utf-8)
(set-default-coding-systems  'utf-8)
(set-keyboard-coding-system  'utf-8-unix)
(set-terminal-coding-system  'utf-8-unix)

;; => Basic Options

;; Relative Line Numbers
(setq display-line-numbers-type 'relative)
(setq display-line-numbers-width-start t)
(setq display-line-numbers-major-tick 10)
(global-display-line-numbers-mode t)

;; Use Spaces instead of Tabs
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Disable Line Wrapping
(setq-default truncate-lines t)

;; Hightlight Cursorline
(global-hl-line-mode t)

;; Inhibit Startup Screen
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")

;; Disable Unnecessary UI-Elements
(menu-bar-mode   -1)
(tool-bar-mode   -1)
(scroll-bar-mode -1)
(tooltip-mode    -1)

;; Change Default Scrolling Behaviour
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position 'always)
(setq mouse-wheel-progressive-speed nil)
(setq scroll-step 1)
(setq scroll-margin 4)

;; Disable Autosaves And Backups
(setq-default auto-save-default nil)
(setq-default create-lockfiles nil)
(setq-default make-backup-files nil)

;; Balanced Windows <https://zck.org/balance-emacs-windows>
(setq window-combination-resize t)

;; Tab For Code Completion
(setq tab-always-indent 'complete)

;; Automaticly Follow Symbolic Links
(setq vc-follow-symlinks t)

;; Prettify Symbols
(global-prettify-symbols-mode t)

;; Enable Deletion Of Selected Text
(delete-selection-mode t)

;; Enable Autopairs
(electric-pair-mode t)

;; Delete Trailling Whitespace On Save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Wrap Around In Minibuffer When Selecting Completion Options
(setq completion-auto-wrap t)

;; Make Background Transparent
(add-to-list 'default-frame-alist '(alpha-background . 85))

;; When Recentering Only Recenter To The Middle Of The Screen
(setq recenter-positions '(middle))

;; Move Cursor By Subword
(global-subword-mode t)

;; Automaticly Kill Process Without Confirmation On Exit
(setq confirm-kill-processes nil)

;; Flyspell
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; :==:> Theming / Appearance

;; => Colorscheming
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-gruvbox t))

;; => Font-Face
(set-face-attribute 'default
                    nil
                    :family "Iosevka"
                    :height 105
                    :weight 'regular)

;; Custom Faces For Org-Mode
(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.6))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.4))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))

;; => Modeline

(custom-set-faces
 '(mode-line          ((t (:box (:line-width 6 :color "black")))))
 '(mode-line-inactive ((t (:box (:line-width 6 :color "gray10"))))))

(setq mode-line-format
      '("%e"

        (:eval (if (buffer-modified-p)
                   " ✏️ "
                 " 💾 "))

        (:eval (format " %s "
                       (propertize (buffer-name)
                                   'face 'italic)))

        (:eval (propertize " %l:%c "
                           'face 'bold))))

(setq-default mode-line-format mode-line-format)

;; :==:> Custom Functions

;; => Switching User Buffers
;; Idea And Code Partially Stolen From <http://xahlee.info/emacs/emacs/elisp_next_prev_user_buffer.html>

(defmacro cef-cycle-through-user-buffers (next-or-prev)
  "Macro For Cycling Through User Buffers."

  `(let (start-in-buffer
         end-in-buffer)

     (setq start-in-buffer (buffer-name))

     (,next-or-prev)

     (let ((i 0))
       (while (< i 20)
         (if (string-equal "*" (substring (buffer-name) 0 1))
             (progn (,next-or-prev)
                    (setq i (+ i 1)))
           (setq i 100
                 end-in-buffer (buffer-name)))))

     (when (eq start-in-buffer end-in-buffer)
       (error "There Is Only One User Buffer Available!"))))

(defun cef-switch-to-next-user-buffer ()
  "Switch To The Next User Buffer."
  (interactive)
  (cef-cycle-through-user-buffers next-buffer))

(defun cef-switch-to-previous-user-buffer ()
  "Switch To The Previous User Buffer."
  (interactive)
  (cef-cycle-through-user-buffers previous-buffer))

;; => Buffer Movement
;; Idea And Code Partially Stolen From <https://github.com/lukhas/buffer-move>

(require 'windmove)

(defun cef-move-window (direction)
  "Helper Function To Move A Window In The Speciefied Direction."

  (let (other-window-p
        buffer-this-buffer)

    (setq other-window-p     (windmove-find-other-window direction)
          buffer-this-buffer (window-buffer (selected-window)))

    (when (or (null other-window-p)
              (and (eq direction 'down)
                   (string-match "^ \\*Minibuf" (buffer-name (window-buffer other-window-p)))))
      (error "Unable To Swap Buffers."))

    (set-window-buffer (selected-window) (window-buffer other-window-p))

    (set-window-buffer other-window-p buffer-this-buffer)
    (select-window other-window-p)))

(defun cef-move-window-up ()
  "Move The Current Window Up."
  (interactive)
  (cef-move-window 'up))

(defun cef-move-window-down ()
  "Move The Current Window Down."
  (interactive)
  (cef-move-window 'down))

(defun cef-move-window-left ()
  "Move The Current Window To The Left."
  (interactive)
  (cef-move-window 'left))

(defun cef-move-window-right ()
  "Move The Current Window To The Right."
  (interactive)
  (cef-move-window 'right))

;; => Killing Buffers

(defun cef-kill-current-buffer ()
  "Kill The Current Buffer."
  (interactive)
  (kill-buffer (buffer-name)))

;; => Open A Terminal

;; Automaticly Kill Buffer When Exiting The Shell
;; <https://stackoverflow.com/questions/12624815/how-to-automatically-kill-buffer-on-terminal-process-exit-in-emacs>
(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  (kill-buffer))

(defun cef-open-terminal ()
  "Open A Terminal Buffer Using The Default User Shell."
  (interactive)
  (term (getenv "SHELL"))
  ;; Deaktivating Settings That Mess Up Terminal Mode
  (display-line-numbers-mode 0)
  (setq-local scroll-margin 0))

;; => Open URL

(defun cef-browse-url-at-line ()
  "Extract A URL From The Current Line And Open It In The Default Web Browser."
  (interactive)
  (let*   ((bounds-of-line (bounds-of-thing-at-point 'line))
           (current-line   (buffer-substring-no-properties (car bounds-of-line) (cdr bounds-of-line)))
           (url-regex      "\\bhttps?://[[:alnum:]#%$&\\?()~_+=/\\.-]+\\b"))
    (if (string-match url-regex current-line)
        (progn
          (setq-local url-as-string (match-string 0 current-line))
          (message (format "Opening `%s` In The Default Web Browser." url-as-string))
          (browse-url url-as-string))
      (error "Unable To Extract URL From Current Line."))))

;; => Inserting Current Date

(defun cef-insert-todays-date ()
  "Insert The Current Date At Cursor Position."
  (interactive)
  (insert
   (format-time-string "%d.%m.%Y")))

;; => Moving Lines
;; Copied From <https://www.emacswiki.org/emacs/MoveLine>

(defmacro cef-save-column (&rest body)
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@body)
       (move-to-column column))))
(put 'cef-save-column 'lisp-indent-function 0)

(defun cef-move-line-up ()
  "Move The Current Line One Line Up."
  (interactive)
  (cef-save-column
    (transpose-lines 1)
    (forward-line -2)))

(defun cef-move-line-down ()
  "Move The Current Line One Line Down."
  (interactive)
  (cef-save-column
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)))

;; => Scroll Up/Down
;; In Order For This To Work The Variable recenter-postions Has To Be Set To Only middle.
;; (setq recenter-positions '(middle)) ;; See Basic Settings ^^

(defun cef-scroll-down ()
  "Scroll Up While Keeping The Cursor In The Center Of The Screen."
  (interactive)
  (scroll-up-command)
  (recenter-top-bottom))

(defun cef-scroll-up ()
  "Scroll Down While Keeping The Cursor In The Center Of The Screen."
  (interactive)
  (scroll-down-command)
  (recenter-top-bottom))

;; => Jump By Paragraph

(defun cef-jump-paragraph-up ()
  "Jump By Paragraph Upwards."
  (interactive)
  (backward-paragraph)
  (recenter-top-bottom))

(defun cef-jump-paragraph-down ()
  "Jump By Paragraph Downwards."
  (interactive)
  (forward-paragraph)
  (recenter-top-bottom))

;; => Split Window

(defun cef-split-window-on-vertical-axis ()
  "Split The Current Window On The Vertical Axis."
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(defun cef-split-window-on-horizontal-axis ()
  "Split The Current Window On The Horizontal Axis."
  (interactive)
  (split-window-vertically)
  (other-window 1))

;; => Delete Text

(defun cef-delete-text ()
  "Delete Text Into Kill Ring If Selected, Otherwise Delete Character."
  (interactive)
  (if mark-active
      (delete-active-region t)
    (delete-char 1)))

;; :==:> Keybindings
;; Custom Keybinding Definitions

;; => Which-Key
;; "Emacs package that displays available keybindings in popup" <https://github.com/justbur/emacs-which-key>
(use-package which-key
  :ensure t
  :init
  (which-key-mode 1)
  :config
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-allow-imprecise-window-fit nil
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 5
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.25
        which-key-idle-delay 0.8
        which-key-max-description-length 40
        which-key-allow-imprecise-window-fit nil
        which-key-separator " → "))

;; => Custom Key-Mappings

;; Window/Buffer Related Keybindings
(defvar-keymap cef-prefix-window-and-buffer-managment
  :doc "Nested Prefix ´C-c w´ for Window Managment Related Commands."
  ; Move Windows In A Specific Direction
  "f" #'cef-move-window-right
  "b" #'cef-move-window-left
  "p" #'cef-move-window-up
  "n" #'cef-move-window-down
  ; Close Windows
  "z" #'delete-other-windows
  "k" #'delete-window
  ; Switch Between Windows
  "o" #'other-window
  ; Split Current Window
  "s" #'cef-split-window-on-horizontal-axis
  "v" #'cef-split-window-on-vertical-axis
  ; Switch Between User-Buffers
  "h" #'cef-switch-to-previous-user-buffer
  "l" #'cef-switch-to-next-user-buffer)

(defvar-keymap cef-prefix-editing-commands
  :doc "C-c e"
  "s" #'replace-string
  "r" #'replace-regexp
  "t" #'replace-rectangle

  "w" #'mark-word
  "p" #'mark-paragraph)

(defvar-keymap cef-prefix-complex-commands
  :doc "C-c C-c"
  "e" #'emoji-insert
  "s" #'scratch-buffer
  "b" #'cef-browse-url-at-line
  "t" #'cef-open-terminal)

;; General Keymap For Custom User Bindings
(defvar-keymap cef-prefix-general-user-bindings
  :doc "Prefix ´C-c´ for General Bindings."
  "k" #'cef-kill-current-buffer
  "p" #'mark-paragraph
  "r" #'rectangle-mark-mode
  "i" #'string-rectangle
  ; Switch Between Windows
  "o" #'other-window
  ; --- Nested Keymaps ---
  "w" cef-prefix-window-and-buffer-managment
  "e" cef-prefix-editing-commands
  "C-c" cef-prefix-complex-commands)

(global-set-key (kbd "C-c") cef-prefix-general-user-bindings)

;; Flyspell-Mode Mappings
(add-hook 'flyspell-mode-hook
          (lambda ()
            (define-key flyspell-mode-map (kbd "C-.") nil)
            (define-key flyspell-mode-map (kbd "M-TAB") nil)

            (define-key flyspell-mode-map (kbd "C-c C-c w") 'flyspell-auto-correct-word)
            ))

;; => Other Bindings
(global-set-key (kbd "C-.") 'repeat)

(global-set-key (kbd "C-,") 'comment-line)

(global-set-key (kbd "C--") 'undo-redo)
(global-set-key (kbd "C-_") 'undo)

(global-set-key (kbd "M-p") 'cef-move-line-up)
(global-set-key (kbd "M-n") 'cef-move-line-down)

(global-set-key (kbd "C-v") 'cef-scroll-down)
(global-set-key (kbd "M-v") 'cef-scroll-up)

(global-set-key (kbd "C-r") 'cef-jump-paragraph-down)
(global-set-key (kbd "M-r") 'cef-jump-paragraph-up)

(global-set-key (kbd "C-d") 'cef-delete-text)

;; => Auto Complete

(use-package auto-complete
  :ensure t
  :init
  (global-auto-complete-mode))

;; => Fuzzy Finding

(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  (vertico-count 15)
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(use-package consult
  :ensure t
  :bind (
         ;; ("C-u"     . consult-buffer)
         ("C-x b"   . consult-buffer)
         ("C-c m b" . consult-buffer)
         ("C-c m t" . consult-theme)
         ("C-c m f" . consult-line)
         ("C-c m y" . consult-yank-pop)))

;; => Language Support

;; List With Languages To Install
(setq cef-languages '(go-mode
                      lua-mode
                      yaml-mode
                      markdown-mode
                      rust-mode
                      csharp-mode
                      dockerfile-mode
                      haskell-mode
                      python-mode))

;; Install All Language-Modes
(dolist (language cef-languages)
  `(use-package ,language :ensure t))
