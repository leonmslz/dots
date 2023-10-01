(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

(setq-default tab-width 4)

(setq-default truncate-lines nil)

(global-hl-line-mode 1)

(delete-selection-mode 1)

(electric-pair-mode 1)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)

(setq inhibit-startup-screen t)
(setq initial-scratch-message "")

(add-to-list 'default-frame-alist '(alpha-background . 90))

(set-frame-font "JetBrains Mono 10" nil t)

(setq scroll-conservatively 10000)

(setq scroll-preserve-screen-position 'always)

(setq scroll-step 1)

(setq scroll-margin 4)

(setq auto-save-mode 0)
(setq make-backup-files nil)

(setf window-combination-resize t)

(setq disabled-command-function nil)

(add-hook 'before-save-hook 'whitespace-cleanup)

(setq completion-auto-wrap t)

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(add-to-list 'custom-theme-load-path "~/.config/emacs/colors")
(load-theme 'everforest-hard-dark t)

(global-set-key (kbd "C-c w s") 'split-window-vertically)
(global-set-key (kbd "C-c w v") 'split-window-horizontally)
(global-set-key (kbd "C-c w k") 'delete-window)
(global-set-key (kbd "C-c w z") 'delete-other-windows)
(global-set-key (kbd "C-c o")   'other-window)
(global-set-key (kbd "C-c C-o") 'other-window)

(defun toggle-selection()
  (interactive)
  (if mark-active
      (keyboard-escape-quit)
    (set-mark (point))))

(defun toggle-rectangle-selection()
  (interactive)
  (if mark-active
      (keyboard-escape-quit)
    (rectangle-mark-mode)))

(global-set-key (kbd "C-SPC") 'toggle-selection)

(global-set-key (kbd "C-c p") 'mark-paragraph)
(global-set-key (kbd "C-c r") 'toggle-rectangle-selection)

(defmacro save-column(&rest body)
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@body)
       (move-to-column column))))
(put 'save-column 'lisp-indent-function 0)

(defun move-line-up()
  (interactive)
  (save-column
    (transpose-lines 1)
    (forward-line -2)))

(defun move-line-down()
  (interactive)
  (save-column
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)))

(global-set-key (kbd "M-p") 'move-line-up)
(global-set-key (kbd "M-n") 'move-line-down)

(global-set-key (kbd "C-.") 'repeat)

(global-set-key (kbd "C-,") 'comment-line)

(global-set-key (kbd "C--") 'undo-redo)
(global-set-key (kbd "C-_") 'undo)

(use-package ivy
  :ensure t
  :diminish
  :init
  (ivy-mode 1)
  :custom
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  (("C-u" . ivy-switch-buffer)))

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
  :ensure)

(add-hook 'org-mode-hook 'org-indent-mode)

(add-hook 'org-mode-hook (lambda () (electric-indent-mode 0)))

(use-package org-modern
  :ensure t
  :config
  (global-org-modern-mode 1))

(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.6))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.4))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
)

(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))
