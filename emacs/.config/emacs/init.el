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

;; => Emacs Server / Deamon

(use-package server
  :ensure t

  :config
  (unless (server-running-p)
	(server-start)))

;; => Options.el
;; Plugin To Group Options Together And Maintain Consistency Across Emacs
;; Regarding The Setting Of Options <https://github.com/leonmslz/options.el>

(use-package options
  :load-path "options.el/"
  :ensure nil

  :config
  (progn

	(op-c line-numbers
		  :desc   "Enable Relative Line Numbers"
		  :set    ((display-line-numbers-type        . relative)
				   (display-line-numbers-width-start . t)
				   (display-line-numbers-major-tick  . 10))
		  :on     (global-display-line-numbers-mode)
		  :hook   (:off (dired-mode-hook
						 eshell-mode-hook
						 term-mode-hook))
		  :ensure t)

	(op-c tabs
		  :desc    "Set Tab Width To 4 Spaces."
		  :set     ((:setdef tab-width . 4))
		  :ensure  t)

	(op-c line-wrapping
		  :desc    "Disable Line Wrapping Globally. If It Is Enabled Behave Like Wrapped Lines Are Seperate Lines."
		  :set     ((:setdef truncate-lines . 1)
					(truncate-partial-width-windows . nil))
		  :on      (visual-line-mode)
		  :ensure  t)

	(op-c hl-line
		  :desc   "Highlight Cursor Line."
		  :on     (global-hl-line-mode)
		  :hook   (:off (term-mode-hook
						 eshell-mode-hook))
		  :ensure t)

	(op-c ui-elements
		  :desc   "Disable Unnecessary UI-Elements And Inhibit Startup Screen"
		  :set    ((inhibit-startup-screen . t)
				   (initial-scratch-message . ""))
		  :off    (menu-bar-mode
				   tool-bar-mode
				   scroll-bar-mode
				   tooltip-mode)
		  :ensure t)

	(op-c scrolling-behaviour
		  :desc  "Change Default Scrolling Behaviour"
		  :set   ((scroll-conservatively . 10000)
				  (scroll-preserve-screen-position . always)
				  (mouse-wheel-progressive-speed . nil)
				  (scroll-step . 1)
				  (scroll-margin . 4))
		  :hook   (:off (term-mode-hook
						 eshell-mode-hook))
		  :ensure t)

	(op-c no-backups
		  :desc   "Disable Autosaves And Backups"
		  :set    ((auto-save-mode . 0)
				   (make-backup-files . nil))
		  :ensure t)

	(op-c balanced-windows
		  :desc   "Balanced Windows <https://zck.org/balance-emacs-windows>"
		  :set    ((window-combination-resize . t))
		  :ensure t)

	(op-c symbolic-links
		  :desc   "Automaticly Follow Symbolic Links"
		  :set    ((vc-follow-symlinks . t))
		  :ensure t)

	(setq disabled-command-function nil)

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

	;; Set Font-Face
	(set-frame-font "JetBrains Mono 10" nil t)

	))

;; :==:> Theming / Appereance

;; => Colorscheming

;; (use-package ef-themes
;;   :ensure t

;;   :config
;;   (load-theme 'ef-spring t))

(add-to-list 'custom-theme-load-path "/home/leon/.config/emacs/visionary-theme.el/")
(load-theme 'visionary-green t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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

(global-set-key (kbd "C-x k") 'cef-kill-current-buffer)

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

(global-set-key (kbd "C-c C-c o i b") 'cef-open-url-in-current-browser-session)
(global-set-key (kbd "C-c C-c o i w") 'cef-open-url-in-new-browser-session)

;; => Copy URL

(defun cef-copy-url ()

  "Interactive Function Which Will Copy All Of The URLs In
   The Current Line Or Region To Your Kill Ring.

   Last Updated: 26.08.2023."

  (interactive)

  (dolist (url
		   (cef-extract-urls-from-string (cef-grap-region-or-line)))
	(kill-new url)))

(global-set-key (kbd "C-c C-c c u") 'cef-copy-url)

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

(global-set-key (kbd "C-SPC") 'toggle-selection)

(global-set-key (kbd "C-c p") 'mark-paragraph)
(global-set-key (kbd "C-c r") 'toggle-rectangle-selection)

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

(global-set-key (kbd "C-c C-c o t") 'cef-open-terminal)

;; => Inserting Current Date

(defun cef-insert-todays-date ()

  "Interactive Function Which Inserts The Current Date.

   Last Updated: 26.08.2023."

  (interactive)

  (insert
   (format-time-string "%d.%m.%Y")))

(global-set-key (kbd "C-c C-c i d") 'cef-insert-todays-date)

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
(global-set-key (kbd "M-p") 'cef-move-line-up)

(defun cef-move-line-down ()
  ""
  (interactive)
  (cef-save-column
	(forward-line 1)
	(transpose-lines 1)
	(forward-line -1)))
(global-set-key (kbd "M-n") 'cef-move-line-down)

;; => Testing Certain Functions

;; Open Dired In A Side Split
(defun cef-dired-side-window ()
  "Open Dired In "
  (interactive)
  (display-buffer-in-side-window
   (dired-noselect default-directory) `((side . left)
										(slot . 0)
										(window-width . 0.2)))
  (other-window))
(global-set-key (kbd "C-c w d") 'cef-dired-side-window)

;; :==:> Keybindings

;; => General.el
;; More Convenient Key Definitions In Emacs <https://github.com/noctuid/general.el>

(use-package general
  :ensure t

  :init
  (require 'general)

  ;; Window And Buffer Managment Related Keybindings
  (general-define-key
   :prefix "C-c"

   ;; Switching User Buffers
   "w c p" 'cef-prev-user-buffer       ;; Switch To Previous User Buffer
   "w c n" 'cef-next-user-buffer       ;; Switch To Next User Buffer

   ;; Splitting Windows
   "w s" 'split-window-vertically      ;; Split The Current Window Horizontally
   "w v" 'split-window-horizontally    ;; Split The Current Window Vertically

   ;; Close Windows
   "w k" 'delete-window                ;; Close The Current Window
   "w z" 'delete-other-windows         ;; Close Every Window Except The Current Window

   ;; Moving Between Splits
   "o"   'other-window                 ;; Switch To The Next Split
   "C-o" 'other-window                 ;; Alias For "C-c o"
   "w f" 'windmove-right               ;; Move Right To The Next Split
   "w b" 'windmove-left                ;; Move Left To The Next Split
   "w n" 'windmove-down                ;; Move Down To The Next Split
   "w p" 'windmove-up                  ;; Move Up To The Next Split

   ;; Moving Buffers Around
   "w m f" 'cef-move-buffer-right      ;; Move The Current Buffer To The Right
   "w m b" 'cef-move-buffer-left       ;; Move The Current Buffer To The Left
   "w m n" 'cef-move-buffer-down       ;; Move The Current Buffer Down
   "w m p" 'cef-move-buffer-up         ;; Move The Current Buffer Up
   )

  ;; Miscellaneous Keybindings
  (general-define-key

   "C-." 'repeat                       ;; Repeat Last Command

   "C-," 'comment-line                 ;; Toggle Comment On Current Line / Region

   "C--" 'undo-redo                    ;; Redo Last Change
   "C-_" 'undo)                        ;; Undo Last Change
)

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("5a3b5c6e95d124fcc23892b97b0a6743376494368fe9566ab8825f637777e693" "b14446ae9fbe8268f6e06f070b62b96b44bb9ad4d10d08848b8eb1a74e6085bc" "6ab94fa8d4bcf530b72674fc8e947db23c044f5e08f34a06ea8eed109c788397" "405e8bbef0f6d8ecd19022ee90fb1234fed049f4b6ed4913e12658ed2fd6dc89" "9f6ba7637b4c3ceb5c697d8dcba8c0e84b506ef11afb439c78b1a8812753116a" "e8ff4936109610b67af0b3d83f95e05224e89a3cb7ada1b4c9c617f3c59319fe" "917599da0c9d9637a95d76a17cf6ecd3e054a4c0236e6d32393a5476b3b6a871" "e116116d2d54141cada3cb72d49d0b5a9aae594a6af86efa2d374b9c793391f7" "73bffcbedeb01f4523d8c9da2715f307aa8627c4a8725037f20a17bc28e1b2d3" "34e207bc4960f5bd23d97c0a8759fec8a97a473b70f9c9c6a5aa2ec9d768d218" "99aef65cee60667ae9d4ff45fe02467aea07b2f31ea618954aa29b392de95103" "4a52fbeb6eb8413b19cd48155b4010670c18706f61765971ba528312db3e811c" default)))
