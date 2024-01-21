;; init.el
;; Configuration File For GNU Emacs
;; By Leon Schulz

;; :==:> Package Managment

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
(setq-default truncate-lines 1)
(setq         truncate-partical-width-windows nil)
;; If Enabled Behave Like Wrapped Lines Are Seperate
(global-visual-line-mode t)

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
(setq-default auto-save-mode 0)
(setq-default make-backup-files nil)

;; Balanced Windows <https://zck.org/balance-emacs-windows>
(setq window-combination-resize t)

;; Automaticly Follow Symbolic Links
(setq vc-follow-symlinks t)

;; Prettify Symbols
(global-prettify-symbols-mode 1)

;; Enable Deletion Of Selected Text
(delete-selection-mode 1)

;; Enable Autopairs
(electric-pair-mode 1)

;; Delete Trailling Whitespace On Save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Wrap Around In Minibuffer When Selecting Completion Options
(setq completion-auto-wrap t)

;; Make Background Transparent
(add-to-list 'default-frame-alist '(alpha-background . 85))

;; :==:> Theming / Appereance

;; => Colorscheming
(use-package ef-themes
  :ensure t
  :config
  (load-theme 'ef-spring t))

;; => Font-Face
(set-face-attribute 'default
                    nil
                    :family "Iosevka"
                    :height 105
                    :weight 'semilight)

;; Custom Faces For Org-Mode
(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.6))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.4))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))

;; :==:> Window And Buffer Managment

;; => Switching User Buffers
;; Idea And Code Partially Stolen From <http://xahlee.info/emacs/emacs/elisp_next_prev_user_buffer.html>

(defmacro cef-cycle-through-user-buffers (next-or-prev)

  "Macro For Cycling Through Your Emacs User Buffers.
  `next-or-prev' Either Takes The Symbol previous-buffer Or next-buffer Which Determines
  Whether It Should Switch To The Next Or Previous User Buffer.

  Last Updated: 25.08.2023."

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

(defun cef-next-user-buffer ()

  "Interactive Function Which Switches To The Next User Buffer
   If Available Otherwise It Will Throw An Error.

   Last Updated: 25.08.2023."

  (interactive)

  (cef-cycle-through-user-buffers next-buffer))

(defun cef-prev-user-buffer ()

  "Interactive Function Which Switches To The Previous User Buffer
   If Available Otherwise It Will Throw An Error.

   Last Updated: 25.08.2023."

  (interactive)

  (cef-cycle-through-user-buffers previous-buffer))

;; => Buffer Movement
;; Idea And Code Partially Stolen From <https://github.com/lukhas/buffer-move>

(require 'windmove)

(defmacro cef-move-buffer (direction)

  "Macro For Swapping The Current Buffer With The Next Buffer
   In The Specified Direction.

   Last Updated: 25.08.2023."

  `(let (other-window-p
         buffer-this-buffer)

     (setq other-window-p     (windmove-find-other-window ',direction)
           buffer-this-buffer (window-buffer (selected-window)))

     (when (or (null other-window-p)
               (and (eq ',direction 'down)
                    (string-match "^ \\*Minibuf" (buffer-name (window-buffer other-win)))))
       (error "Unable To Swap Buffers."))

     (set-window-buffer (selected-window) (window-buffer other-window-p))

     (set-window-buffer other-window-p buffer-this-buffer)
     (select-window other-window-p)))

(defun cef-move-buffer-up ()

  "Interactive Function Which Swaps The Current Buffer With
   The Buffer Above The Split.
   If There Is No Split This Function Will Throw An Error.

   Last Updated: 25.08.2023."

  (interactive)

  (cef-move-buffer up))

(defun cef-move-buffer-down ()

  "Interactive Function Which Swaps The Current Buffer With
   The Buffer Below The Split.
   If There Is No Split This Function Will Thow An Error.

   Last Updated: 25.08.2023."

  (interactive)

  (cef-move-buffer down))

(defun cef-move-buffer-left ()

  "Interactive Function Which Swaps The Current Buffer With
   The Buffer On The Left Of The Split.
   If There Is No Split This Function Will Throw An Error.

   Last Updated: 25.08.2023."

  (interactive)

  (cef-move-buffer left))

(defun cef-move-buffer-right ()

  "Interactive Function Which Swaps The Current Buffer With
   The Buffer On The Right Of The Split.
   If There Is No Split This Function Will Throw An Error.

   Last Updated: 25.08.2023."

  (interactive)

  (cef-move-buffer right))

;; => Killing Buffers

(defun cef-kill-current-buffer ()

  "Interactive Function Which Kills The Current Buffer.

   Last Updated: 25.08.2023."

  (interactive)

  (kill-buffer (buffer-name)))

;; :==:> Custom Functions

;; From <https://emacs.stackexchange.com/questions/7148/get-all-regexp-matches-in-buffer-as-a-list>
(defun cef-multiple-matches (regexp string)

  "Function Which Allows You To Match A Regular Expression Against Multiple
   Occurrences In A Certain String And Returns Them As A List.

   Last Updated: 26.08.2023."

  (save-match-data

    (let ((pos 0)
          matches)

      (while (string-match regexp string pos)
        (push (match-string 0 string) matches)
        (setq pos (match-end 0)))

      matches)))

(defun cef-grap-region-or-line ()

  "Function Which Checks If There Is A Selected Region Active.
   If That Is The Case It Will Return The Text From That Region.
   Otherwise It Will Return The Text From The Current Line.

   Last Updated: 26.08.2023."

  (if mark-active

      (buffer-substring-no-properties
       (region-beginning)
       (region-end))

    (buffer-substring-no-properties
     (point-at-bol)
     (point-at-eol))))

(defun cef-extract-urls-from-string (string)

  "Function Which Extracts All Of The URLs That Are Part
   Of A Given String.

   Last Updated: 26.08.2023."

  (let (url-regexp)

    (setq url-regexp
          "\\b\\(https?://\\|www\\.\\)[-A-Za-z0-9+&@#/%?=~_|!:,.;]*[-A-Za-z0-9+&@#/%=~_|]")

  (cef-multiple-matches url-regexp string)))

;; => Open URL In Browser

(defvar cef-browser "firefox"

  "Global Variable Which Holds The Preffered Browser As A String.")

(defmacro cef-open-url-with (browser-cmd)

  "Macro Which Allows You To Grab All Of The URLs That Are Part
   Of The Current Region Or Line And Opens Them Using A Given
   Browser-Command.

   Last Updated: 26.08.2023."

  `(dolist (url
           (cef-extract-urls-from-string (cef-grap-region-or-line)))

    (shell-command
     (format "%s \"%s\"" ,browser-cmd url))))

(defun cef-open-url-in-current-browser-session ()

  "Interactive Function Which Opens One Or Multiple URLs In
   The Current Browser Sesssion.
   If There Is No Session Availeble It Will Open A New One.

   Last Updated: 26.08.2023."

  (interactive)

  (cef-open-url-with cef-browser))

(defun cef-open-url-in-new-browser-session ()

  "Interactive Function Which Opens One Or Multiple URLs In
   A New Browser Sesssion.

   Last Updated: 26.08.2023."

  (interactive)

  (cef-open-url-with
   (concat cef-browser " -new-window")))

;; => Copy URL

(defun cef-copy-url ()

  "Interactive Function Which Will Copy All Of The URLs In
   The Current Line Or Region To Your Kill Ring.

   Last Updated: 26.08.2023."

  (interactive)

  (dolist (url
           (cef-extract-urls-from-string (cef-grap-region-or-line)))
    (kill-new url)))

;; => Selecting Text

(defun toggle-selection ()
  ""
  (interactive)
  (if mark-active
      (keyboard-escape-quit)
    (set-mark (point))))

(defun toggle-rectangle-selection ()
  ""
  (interactive)
  (if mark-active
      (keyboard-escape-quit)
    (rectangle-mark-mode)))

;; => Open A Terminal In The Current Pane

(defun cef-open-terminal ()

  "Interactive Funtion Which Allows You To Open A Terminal Buffer
   Using The Default User Shell.

   Last Updated: 26.08.2023."

  (interactive)

  ;; Open A Terminal Buffer Using The Default User Shell
  (term (getenv "SHELL"))

  (define-key term-mode-map (kbd "M-x") 'execute-extended-command)

  ;; Automaticly Kill Buffer When Exiting The Shell
  ;; <https://stackoverflow.com/questions/12624815/how-to-automatically-kill-buffer-on-terminal-process-exit-in-emacs>
  (defadvice term-handle-exit
      (after term-kill-buffer-on-exit activate)
    (kill-buffer)))

;; => Inserting Current Date

(defun cef-insert-todays-date ()

  "Interactive Function Which Inserts The Current Date.

   Last Updated: 26.08.2023."

  (interactive)

  (insert
   (format-time-string "%d.%m.%Y")))

;; => Moving Lines
;; Copied From <https://www.emacswiki.org/emacs/MoveLine>

(defmacro cef-save-column (&rest body)
  ""
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@body)
       (move-to-column column))))
(put 'cef-save-column 'lisp-indent-function 0)

(defun cef-move-line-up ()
  ""
  (interactive)
  (cef-save-column
    (transpose-lines 1)
    (forward-line -2)))

(defun cef-move-line-down ()
  ""
  (interactive)
  (cef-save-column
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)))

;; :==:> Keybindings

;; Custom Keybinding Definitions

(global-set-key (kbd "C-c w s")			'split-window-vertically)
(global-set-key (kbd "C-c w v")			'split-window-horizontally)

(global-set-key (kbd "C-c w k")			'delete-window)
(global-set-key (kbd "C-c w z")			'delete-other-windows)

(global-set-key (kbd "C-c o")			'other-window)

(global-set-key (kbd "C-c w f")			'cef-move-buffer-right)
(global-set-key (kbd "C-c w b")			'cef-move-buffer-left)
(global-set-key (kbd "C-c w n")			'cef-move-buffer-down)
(global-set-key (kbd "C-c w p")			'cef-move-buffer-up)

(global-set-key (kbd "C-.")				'repeat)

(global-set-key (kbd "C-,")				'comment-line)

(global-set-key (kbd "C--")				'undo-redo)
(global-set-key (kbd "C-_")				'undo)

(global-set-key (kbd "M-p")				'cef-move-line-up)
(global-set-key (kbd "M-n")				'cef-move-line-down)

(global-set-key (kbd "C-c C-c i d")		'cef-insert-todays-date)
(global-set-key (kbd "C-c C-c o t")		'cef-open-terminal)

(global-set-key (kbd "C-SPC")			'toggle-selection)

(global-set-key (kbd "C-c p")			'mark-paragraph)
(global-set-key (kbd "C-c r")			'toggle-rectangle-selection)

(global-set-key (kbd "C-c C-c c u")		'cef-copy-url)

(global-set-key (kbd "C-c C-c o i b")	'cef-open-url-in-current-browser-session)
(global-set-key (kbd "C-c C-c o i w")	'cef-open-url-in-new-browser-session)

(global-set-key (kbd "C-x k")			'cef-kill-current-buffer)

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
  :bind (("C-u"     . consult-buffer)
         ("C-x b"   . consult-buffer)
         ("C-c m b" . consult-buffer)
         ("C-c m t" . consult-theme)
         ("C-c m f" . consult-line)
         ("C-c m y" . consult-yank-pop)))

;; => Language Support

(use-package go-mode
  :ensure t)

(use-package lua-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package csharp-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package haskell-mode
  :ensure t)

(use-package python-mode
  :ensure t)

;; => Org Mode

(add-hook 'org-mode-hook 'org-indent-mode)

(add-hook 'org-mode-hook (lambda () (electric-indent-mode 0)))

(use-package org-modern
  :ensure t
  :init
  (global-org-modern-mode 1))

(use-package rainbow-mode
  :ensure t
  :init
  (rainbow-mode 1))

;; => Dired

(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))
