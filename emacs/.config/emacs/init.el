;; init.el
;; Configuration File For GNU Emacs
;; By Leon Schulz

;; => General Settings

;; Display Relative Line Numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

;; Set Tab Width To 4 Spaces
(setq-default tab-width 4)

;; Disable Line Wrapping
(setq-default truncate-lines 1)
(setq truncate-partial-width-windows nil)
;; If It Is Enabled Behave Like Wrapped Are Seperate Lines
(visual-line-mode 1)

;; Hightlight Cursor Line
(global-hl-line-mode 1)

;; Enable Deletion Of Selected Text
(delete-selection-mode 1)

;; Enable Autopairs
(electric-pair-mode 1)

;; Disable Unnecessary UI-Elements
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)

;; Inhibit Startup Screen
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")

;; Make Background Transparent
(add-to-list 'default-frame-alist '(alpha-background . 90))

;; Set Font-Face
(set-frame-font "JetBrains Mono 10" nil t)

;; Reproduce Vim Scrolling Behaviour
(setq scroll-conservatively 10000)

;; Preserve Screen Position When Scrolling With The Mouse
(setq scroll-preserve-screen-position 'always)

;; Make Scrolling Smoother By Only Scrolling One Line At A Time
(setq scroll-step 1)

;; Set A Scroll Offset Of 4 Lines
(setq scroll-margin 4)

;; Disable Autosaves And Backups
(setq auto-save-mode 0)
(setq make-backup-files nil)

;; Balanced Windows
;; <https://zck.org/balance-emacs-windows>
(setf window-combination-resize t)

(setq disabled-command-function nil)

;; Automaticly Follow Symbolic Links
(setq vc-follow-symlinks t)

;; Delete Trailling Whitespace On Save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Wrap Around In Minibuffer When Selecting Completion Options
(setq completion-auto-wrap t)

;; => Package Managment

;; MELPA
;; <https://melpa.org/>
(require 'package)
;; (add-to-list 'package-archives
;;			 '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
			 '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; use-package
;; <https://github.com/jwiegley/use-package>
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; => Colorscheme

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (load-theme 'doom-zenburn t))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1))

(custom-set-faces
 '(font-lock-comment-face ((t (:foreground "#859289" :inherit italic))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.6))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.4))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))

;; => Window And Buffer Managment

;; Switching User Buffers
;; Code Partially Stolen From <http://xahlee.info/emacs/emacs/elisp_next_prev_user_buffer.html>
(defun cef-next-user-buffer ()
  "Switch To The Next User Buffer."
  (interactive)
  (setq start (buffer-name))
  (next-buffer)
  (let ((i 0))
	(while (< i 20)
	  (if (string-equal "*" (substring (buffer-name) 0 1))
		  (progn (next-buffer)
				 (setq i (+ i 1)))
		(setq i 100
			  dest (buffer-name)))))
  (when (eq start dest)
	(error "There Is Only One User-Buffer Available!")))
(global-set-key (kbd "C-c w n") 'cef-next-user-buffer)

(defun cef-prev-user-buffer ()
  "Switch To The Previous User Buffer."
  (interactive)
  (setq start (buffer-name))
  (previous-buffer)
  (let ((i 0))
	(while (< i 20)
	  (if (string-equal "*" (substring (buffer-name) 0 1))
		  (progn (previous-buffer)
				 (setq i (+ i 1)))
		(setq i 100
			  dest (buffer-name)))))
  (when (eq start dest)
	(error "There Is Only One User-Buffer Available!")))
(global-set-key (kbd "C-c w p") 'cef-prev-user-buffer)

;; Buffer Movement
;; Stolen From This Repository: <https://github.com/lukhas/buffer-move>
(require 'windmove)

(defun cef-buffer-move-up ()
  "Swap the current buffer and the buffer above the split.
If there is no split, ie now window above the current one, an
error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'up))
	 (buf-this-buf (window-buffer (selected-window))))
	(if (null other-win)
		(error "No window above this one")
	  ;; swap top with this one
	  (set-window-buffer (selected-window) (window-buffer other-win))
	  ;; move this one to top
	  (set-window-buffer other-win buf-this-buf)
	  (select-window other-win))))

(defun cef-buffer-move-down ()
"Swap the current buffer and the buffer under the split.
If there is no split, ie now window under the current one, an
error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'down))
	 (buf-this-buf (window-buffer (selected-window))))
	(if (or (null other-win)
			(string-match "^ \\*Minibuf" (buffer-name (window-buffer other-win))))
		(error "No window under this one")
	  ;; swap top with this one
	  (set-window-buffer (selected-window) (window-buffer other-win))
	  ;; move this one to top
	  (set-window-buffer other-win buf-this-buf)
	  (select-window other-win))))

(defun cef-buffer-move-left ()
"Swap the current buffer and the buffer on the left of the split.
If there is no split, ie now window on the left of the current
one, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'left))
	 (buf-this-buf (window-buffer (selected-window))))
	(if (null other-win)
		(error "No left split")
	  ;; swap top with this one
	  (set-window-buffer (selected-window) (window-buffer other-win))
	  ;; move this one to top
	  (set-window-buffer other-win buf-this-buf)
	  (select-window other-win))))

(defun cef-buffer-move-right ()
"Swap the current buffer and the buffer on the right of the split.
If there is no split, ie now window on the right of the current
one, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'right))
	 (buf-this-buf (window-buffer (selected-window))))
	(if (null other-win)
		(error "No right split")
	  ;; swap top with this one
	  (set-window-buffer (selected-window) (window-buffer other-win))
	  ;; move this one to top
	  (set-window-buffer other-win buf-this-buf)
	  (select-window other-win))))

(global-set-key (kbd "C-c w f") 'cef-buffer-move-right)
(global-set-key (kbd "C-c w b") 'cef-buffer-move-left)
(global-set-key (kbd "C-c w n") 'cef-buffer-move-down)
(global-set-key (kbd "C-c w p") 'cef-buffer-move-up)

(global-set-key (kbd "C-c C-c w f") 'windmove-right)
(global-set-key (kbd "C-c C-c w b") 'windmove-left)
(global-set-key (kbd "C-c C-c w n") 'windmove-down)
(global-set-key (kbd "C-c C-c w p") 'windmove-up)

;; Kill Current Buffer
(defun cef-kill-current-buffer ()
  "Kill The Current Buffer."
  (interactive)
  (kill-buffer (buffer-name)))
(global-set-key (kbd "C-x k") 'cef-kill-current-buffer)

;; General
(use-package window
  :bind (("C-c w s" . split-window-vertically)
		 ("C-c w v" . split-window-horizontally)
		 ("C-c w k" . delete-window)
		 ("C-c w z" . delete-other-windows)
		 ("C-c o"   . other-window)
		 ("C-c C-o" . other-window)))

;; Open URL in Browser
(defvar cef-browser-command "firefox"
  "Contains The Name Of The Default Browser To Open Links With.
   (Defaults To: 'firefox')")

(defvar cef-url-regex "\\b\\(https?://\\|www\\.\\)[-A-Za-z0-9+&@#/%?=~_|!:,.;]*[-A-Za-z0-9+&@#/%=~_|]"
  "RegEx To Match URLs.")

;; From <https://emacs.stackexchange.com/questions/7148/get-all-regexp-matches-in-buffer-as-a-list>
(defun cef-multiple-matches (regexp string)
  "Return A List Containing All Of The Elements That Match A Given RegEx In A Certain String."
  (save-match-data
	(let ((pos 0)
		  matches)
	  (while (string-match regexp string pos)
		(push (match-string 0 string) matches)
		(setq pos (match-end 0)))
	  matches)))

(defun cef-grap-region-or-line ()
  "Function That Returns Either The Current Region Or Unless There Is A Active Mark The Current Line As A String."
  (if mark-active
	  (buffer-substring-no-properties (region-beginning) (region-end))
	(buffer-substring-no-properties (point-at-bol) (point-at-eol))))

(defun cef-extract-urls-from-string (string)
  "Extract All URLs From A Given String."
  (cef-multiple-matches cef-url-regex string))

(defun cef-open-url-with (browser-cmd)
  ""
  (interactive)
	(dolist (url (cef-extract-urls-from-string (cef-grap-region-or-line)))
	  (shell-command (format "%s \"%s\"" browser-cmd url))))

(global-set-key (kbd "C-c C-c o i b") (lambda ()
										""
										(interactive)
										(cef-open-url-with cef-browser-command)))

(global-set-key (kbd "C-c C-c o i w") (lambda ()
										""
										(interactive)
										(cef-open-url-with (concat cef-browser-command " -new-window"))))

(defun cef-copy-url ()
  ""
  (interactive)
  (dolist (url (cef-extract-urls-from-string (cef-grap-region-or-line)))
	(kill-new url)))
(global-set-key (kbd "C-c C-c c u") 'cef-copy-url)

;; Text Selection
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

;; Open A Terminal
(defun cef-open-terminal ()
  "Open A Terminal Buffer With The Default User Shell And Disable Line Numbers."
  (interactive)
  (term (getenv "SHELL"))
  (display-line-numbers-mode 0)
  (setq-local scroll-conservatively 0
			  scroll-margin 0)
  ;; Automaticly Kill Buffer When Exiting Shell
  ;; <https://stackoverflow.com/questions/12624815/how-to-automatically-kill-buffer-on-terminal-process-exit-in-emacs>
  (defadvice term-handle-exit
	  (after term-kill-buffer-on-exit activate)
	(kill-buffer)))
(global-set-key (kbd "C-c C-c o t") 'cef-open-terminal)

;; Moving Lines
;; Copied From <https://www.emacswiki.org/emacs/MoveLine>
(defmacro save-column (&rest body)
  ""
  `(let ((column (current-column)))
	 (unwind-protect
		 (progn ,@body)
	   (move-to-column column))))
(put 'save-column 'lisp-indent-function 0)

(defun move-line-up ()
  ""
  (interactive)
  (save-column
	(transpose-lines 1)
	(forward-line -2)))
(global-set-key (kbd "M-p") 'move-line-up)

(defun move-line-down ()
  ""
  (interactive)
  (save-column
	(forward-line 1)
	(transpose-lines 1)
	(forward-line -1)))
(global-set-key (kbd "M-n") 'move-line-down)

(defun cef-dired-side-window ()
  "Open Dired In "
  (interactive)
  (display-buffer-in-side-window
   (dired-noselect default-directory) `((side . left)
										(slot . 0)
										(window-width . 0.2)))
  (other-window))
(global-set-key (kbd "C-c w d") 'cef-dired-side-window)

;; Other Keybindings
(global-set-key (kbd "C-.") 'repeat)

(global-set-key (kbd "C-,") 'comment-line)

(global-set-key (kbd "C--") 'undo-redo)
(global-set-key (kbd "C-_") 'undo)

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
  :bind (("C-u"		. consult-buffer)
		 ("C-x b"	. consult-buffer)
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
