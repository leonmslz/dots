;; -*- lexical-binding: t -*-
;; init.el
;;  ______                             _____             __ _
;; |  ____|                           / ____|           / _(_)
;; | |__   _ __ ___   __ _  ___ ___  | |     ___  _ __ | |_ _  __ _
;; |  __| | '_ ` _ \ / _` |/ __/ __| | |    / _ \| '_ \|  _| |/ _` |
;; | |____| | | | | | (_| | (__\__ \ | |___| (_) | | | | | | | (_| |
;; |______|_| |_| |_|\__,_|\___|___/  \_____\___/|_| |_|_| |_|\__, |
;;                                                             __/ |
;;                                                            |___/
;; Configuration File For GNU Emacs
;; By Leon Schulz

(defgroup cef-emacs-configuration nil
  "Custom GNU Emacs Configuration By Leon Schulz."
  :prefix 'cef)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs On-Startup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; => Emacs Server / Daemon

(use-package server
  :ensure t
  :config
  (unless (server-running-p)
    (server-start)))

;; Emacs Default Frame Size

(when window-system
  (set-frame-size (selected-frame) 160 55))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; UTF-8 As Default Encoding
(set-language-environment    'utf-8)
(set-default-coding-systems  'utf-8)
(set-keyboard-coding-system  'utf-8-unix)
(set-terminal-coding-system  'utf-8-unix)

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

;; Change Default Scrolling Behavior
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position 'always)
(setq mouse-wheel-progressive-speed nil)
(setq scroll-step 1)
(setq scroll-margin 4)

;; Disable Autosaves And Backups
(setq-default auto-save-default nil)
(setq-default create-lockfiles nil)
(setq-default make-backup-files nil)

;; Stop Emacs From Editing The Configuration File
(setq custom-file "/dev/null")

;; Disable Ring Bell
(setq ring-bell-function 'ignore)

;; Automatically Revert Buffer On Changes
(global-auto-revert-mode t)

;; Balanced Windows <https://zck.org/balance-emacs-windows>
(setq window-combination-resize t)

;; Text-Scale Adjust Step
(setq-default text-scale-mode-amount 1.1)

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
(set-frame-parameter nil 'alpha-background 75)
(add-to-list 'default-frame-alist '(alpha-background . 75))

;; When Recentering Only Recenter To The Middle Of The Screen
(setq recenter-positions '(middle))

;; Move Cursor By Subword
(global-subword-mode t)

;; Automaticly Kill Process Without Confirmation On Exit
(setq confirm-kill-processes nil)

;; Flyspell
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Dired
(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-listing-switches "-al --group-directories-first")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theming / Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; :==:> Colorscheme

(use-package ef-themes
  :ensure t
  :config
  (load-theme 'ef-elea-dark t))

;; :==:> Font-Face

(face-attribute 'default :height)

(defcustom cef-default-font-size 10.5
  "Default Font Size For All Emacs UI-Elements."
  :group 'cef-emacs-configuration)

(defcustom cef-huge-font-size 12.0
  "Huge Font Size For All Emacs UI-Elements."
  :group 'cef-emacs-configuration)

(set-face-attribute 'default
                    nil
                    :family "Iosevka"
                    :height (round (* 10 cef-default-font-size))
                    :weight 'regular)

;; Custom Faces For Org-Mode
(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.6))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.4))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))

;; Mode-Line

(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))
(require 'mode-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun cef-disable-settings-locally ()
  (interactive)
  )

;; Deactivating Settings That Mess Up Terminal Mode
(add-hook 'term-mode-hook
          (lambda ()
            (display-line-numbers-mode 0)
            (setq-local scroll-margin 0)
            (set (make-local-variable 'global-hl-line-mode) nil)))

(add-hook 'help-mode-hook
          (lambda ()
            (display-line-numbers-mode 0)))

(defun cef-open-terminal ()
  "Open A Terminal Buffer Using The Default User Shell."
  (interactive)
  (term (getenv "SHELL"))
  ;; Rename Terminal Buffer So That The User Can Open Multiple Term-Buffers
  (rename-buffer (format "%s %s" (buffer-name) (format-time-string "%s")) t))

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
  (backward-paragraph))
  ;; (recenter-top-bottom))

(defun cef-jump-paragraph-down ()
  "Jump By Paragraph Downwards."
  (interactive)
  (forward-paragraph))
  ;; (recenter-top-bottom))

;; => Auto-create Missing Directories
;; Reference: <https://emacsredux.com/blog/2022/06/12/auto-create-missing-directories/>

;; Alternative To The Code Below Using An Advice:
; (defun cef-auto-create-missing-dirs-find-file (orig-fun &rest args)
;   (let* ((filename (car args))
;          (target-dir (file-name-directory filename)))
;     (unless (file-directory-p target-dir)
;       (make-directory target-dir t))
;     (apply orig-fun args)))
; (advice-add 'find-file :around #'cef-auto-create-missing-dirs-find-file)

(defun cef-auto-create-missing-dirs-find-file ()
  (let ((target-dir (file-name-directory buffer-file-name)))
    (unless (file-exists-p target-dir)
      (make-directory target-dir t))))

(add-to-list 'find-file-not-found-functions #'cef-auto-create-missing-dirs-find-file)

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

;; => Apply Default Font-Size

(defun cef-apply-default-font-size ()
  "Set The Font Size Of All Emacs UI-Elements Back To The Value Of 'cef-default-font-size."
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height (round (* 10 cef-default-font-size))))

(defun cef-apply-huge-font-size ()
  "Set The Font Size Of All Emacs UI-Elements Back To The Value Of 'cef-default-font-size."
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height (round (* 10 cef-huge-font-size))))

;; => Dired Related Functions

;; Open Dired In Place
(defun cef-open-dired-current-directory ()
  "Open Dired In Place."
  (interactive)
  (dired "."))

;; Open Dired In Home Folder
(defun cef-open-dired-home-directory ()
  "Open Dired In Home Folder."
  (interactive)
  (dired "~"))

;; Go Up A Directory
(defun cef-dired-directory-up ()
  "Move Up A Directory."
  (interactive)
  (find-alternate-file ".."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additional Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; => All the Icons

(use-package all-the-icons-dired
  :ensure t)

(add-hook 'dired-mode-hook
          (lambda ()
            (display-line-numbers-mode 0)
            (setq-local scroll-margin 0)
            (all-the-icons-dired-mode t)))

;; => Auto Complete

(use-package auto-complete
  :ensure t
  :init
  (global-auto-complete-mode))

(use-package rainbow-mode
  :ensure t)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language Support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; List With Languages To Install
(setq cef-languages '(go-mode
                      lua-mode
                      yaml-mode
                      markdown-mode
                      rust-mode
                      csharp-mode
                      dockerfile-mode
                      haskell-mode
                      nix-mode
                      clojure-mode
                      python-mode
                      git-modes
                      ))

;; Install All Language-Modes
(dolist (language cef-languages)
  (unless (package-installed-p language)
    (package-install language)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Key-Binding Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; => C-c Keybindings

;; Window/Buffer Related Keybindings
(defvar-keymap cef-prefix-window-and-buffer-management
  :doc "Nested Prefix ´C-c w´ for Window Management Related Commands."
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

;; Keybindings For Manipulating Text
(defvar-keymap cef-prefix-editing-commands
  :doc "C-c e"
  "s" #'replace-string
  "r" #'replace-regexp
  "t" #'replace-rectangle

  "a" #'align-regexp

  "w" #'mark-word
  "p" #'mark-paragraph)

;; More Verbose Keybindings
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
  ; Dired
  "d" #'cef-open-dired-current-directory
  "h" #'cef-open-dired-home-directory
  ; Switch Between Windows
  "o" #'other-window
  ; --- Nested Keymaps ---
  "w" cef-prefix-window-and-buffer-management
  "e" cef-prefix-editing-commands
  "C-c" cef-prefix-complex-commands)

(global-set-key (kbd "C-c") cef-prefix-general-user-bindings)

;; => Major Mode Specific Bindings

;; Flyspell-Mode Mappings
(require 'flyspell)
(defvar-keymap cef-flyspell-mode
  :doc    "Custom Keybindings For Flyspell Major Mode."
  :keymap flyspell-mode-map
  ; Disable Flyspell Keys That Conflict With Other Bindings.
  "C-."   nil
  "C-,"   nil
  "M-TAB" nil
  ; Auto-Correct
  "C-c C-c w" #'flyspell-auto-correct-word)

;; Dired Keybindings
(require 'dired)
(defvar-keymap cef-dired-mode
  :doc    "Custom Keybindings For Dired Major Mode."
  :keymap dired-mode-map
  "u" #'cef-dired-directory-up)

;; Term-Mode Keybindings
(require 'term)
(defvar-keymap cef-term-mode
  :doc    "Custom Keybindings For Term Major Mode."
  :keymap term-raw-map
  ; Disable Term-Mode Keys That Conflict With Other Bindings.
  "C-c w" nil)

;; => First Level Global Bindings

(defvar-keymap cef-global-mode
  :doc ""
  :keymap global-map

  "C-." #'repeat

  "C-," #'comment-line

  "C--" #'undo-redo
  "C-_" #'undo

  "M-p" #'cef-move-line-up
  "M-n" #'cef-move-line-down

  "C-v" #'cef-scroll-down
  "M-v" #'cef-scroll-up

  "C-r" #'cef-jump-paragraph-down
  "M-r" #'cef-jump-paragraph-up

  "C-d" #'cef-delete-text
  ; Global Font-Size Adjustments
  "C-x C-+" #'global-text-scale-adjust
  "C-x C--" #'global-text-scale-adjust
  "C-x C-0" #'cef-apply-default-font-size
  "C-x C-1" #'cef-apply-huge-font-size)
