;; -*- lexical-binding: t -*-
;; bindings.el - Key-Binding Definitions

;; --- Packages ---

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

;; --- Custom Keybinding-Definitions ---

;; Window/Buffer Related Keybindings
(defvar-keymap cef-prefix-window-and-buffer-management
  :doc "Nested Prefix ´C-c w´ for Window Management Related Commands."
  ;; Move Windows In A Specific Direction
  "f" #'cef-move-window-right
  "b" #'cef-move-window-left
  "p" #'cef-move-window-up
  "n" #'cef-move-window-down
  ;; Close Windows
  "z" #'delete-other-windows
  "k" #'delete-window
  ;; Switch Between Windows
  "o" #'other-window
  ;; Split Current Window
  "s" #'cef-split-window-on-horizontal-axis
  "v" #'cef-split-window-on-vertical-axis
  ;; Winner Mode
  "u" #'winner-undo
  "r" #'winner-redo
  ;; Switch Between User-Buffers
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
  ;; Dired
  "d" #'cef-open-dired-current-directory
  "h" #'cef-open-dired-home-directory
  ;; Switch Between Windows
  "o" #'other-window
  ;; --- Nested Keymaps ---
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
  ;; Disable Flyspell Keys That Conflict With Other Bindings.
  "C-."   nil
  "C-,"   nil
  "M-TAB" nil
  ;; Auto-Correct
  "C-c C-c w" #'flyspell-auto-correct-word)

;; Dired Keybindings
(require 'dired)
(defvar-keymap cef-dired-mode
  :doc    "Custom Keybindings For Dired Major Mode."
  :keymap dired-mode-map
  "u" #'cef-dired-directory-up
  "x" #'cef-dired-toggle-execute-permission
  "<mouse-2>" #'dired-find-file)

;; Term-Mode Keybindings
(require 'term)
(defvar-keymap cef-term-mode
  :doc    "Custom Keybindings For Term Major Mode."
  :keymap term-raw-map
  ;; Disable Term-Mode Keys That Conflict With Other Bindings.
  "C-c" nil
  "C-x" nil
  "M-x" nil)

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

  "C-r" #'forward-paragraph
  "M-r" #'backward-paragraph

  "C-d" #'cef-delete-text
  ;; Global Font-Size Adjustments
  "C-x C-+" #'global-text-scale-adjust
  "C-x C--" #'global-text-scale-adjust
  "C-x C-0" #'cef-hard-reset-font-size)

;; --- Export ---
(provide 'bindings)
