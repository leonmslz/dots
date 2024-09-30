;; basics.el --- Must Have Emacs Settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; --- Settings ---

;; Emacs Server
(use-package server
  :ensure t
  :config
  (unless (server-running-p)
    (server-start)))

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

;; Change Default Scrolling Behavior
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 'always)
(setq mouse-wheel-progressive-speed nil)
(setq scroll-step 1)
(setq scroll-margin 4)

;; Disable Autosaves And Backups
(setq-default auto-save-default nil)
(setq-default create-lockfiles  nil)
(setq-default make-backup-files nil)

;; Stop Emacs From Editing The Configuration File
(setq custom-file "/dev/null")

;; Suppress Compiler Warnings
(setq native-comp-async-report-warnings-errors 'silent)

;; Disable Ring Bell
(setq ring-bell-function 'ignore)

;; Automatically Revert Buffer On Changes
(global-auto-revert-mode t)

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
(set-frame-parameter nil 'alpha-background 85)
(add-to-list 'default-frame-alist '(alpha-background . 85))

;; When Recentering Only Recenter To The Middle Of The Screen
(setq recenter-positions '(middle))

;; Move Cursor By Subword
(global-subword-mode t)

;; Automaticly Kill Process Without Confirmation On Exit
(setq confirm-kill-processes nil)

;; Simply Ask For Y Or N Instead Of Yes Or No
(defalias 'yes-or-no-p 'y-or-n-p)

;; Flyspell
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Deactivate Settings In Certain Major-Modes

(defun cef-add-hook (f hooks)
  "Add A Given Function F To Multiple HOOKS."
  (mapc (lambda (hook) (add-hook hook f))
        hooks))

(cef-add-hook (lambda () (display-line-numbers-mode 0))
              '(help-mode-hook
                Info-mode-hook
                term-mode-hook
                eshell-mode-hook
                dired-mode-hook))

(cef-add-hook (lambda () (setq-local scroll-margin 0))
              '(term-mode-hook))

(cef-add-hook (lambda () (set (make-local-variable 'global-hl-line-mode) nil))
              '(term-mode-hook))

;; --- Export ---
(provide 'basics)

;;; basics.el ends here
