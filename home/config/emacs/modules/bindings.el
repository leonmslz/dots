;; bindings.el --- Key-Binding Definitions -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

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

;; --- External Functions ---
;; Pre-Declaration Of Externally Defined Functions To Maintain Clear, Warning-Free Compilations

(declare-function cef-move-window-right               "buffer-management")
(declare-function cef-move-window-left                "buffer-management")
(declare-function cef-move-window-up                  "buffer-management")
(declare-function cef-move-window-down                "buffer-management")
(declare-function cef-split-window-on-horizontal-axis "buffer-management")
(declare-function cef-split-window-on-vertical-axis   "buffer-management")
(declare-function winner-undo                         "buffer-management")
(declare-function winner-redo                         "buffer-management")
(declare-function cef-switch-to-previous-user-buffer  "buffer-management")
(declare-function cef-switch-to-next-user-buffer      "buffer-management")
(declare-function cef-browse-url-at-line              "")
(declare-function cef-open-terminal                   "")
(declare-function cef-kill-current-buffer             "")
(declare-function cef-open-dired-current-directory    "")
(declare-function cef-open-dired-home-directory       "")
(declare-function cef-dired-directory-up              "")
(declare-function cef-dired-toggle-execute-permission "")
(declare-function cef-dired-toggle-mark               "")
(declare-function cef-move-line-up                    "")
(declare-function cef-move-line-down                  "")
(declare-function cef-scroll-down                     "")
(declare-function cef-scroll-up                       "")
(declare-function cef-delete-text                     "")
(declare-function cef-hard-reset-font-size            "")
(declare-function cef-transpose-paragraphs            "")

;; --- Custom Keybinding-Definitions ---

;; Window/Buffer Related Keybindings
(defvar-keymap cef-prefix-window
  :doc    "Nested Prefix ´C-c w´ for Window Management Related Commands."
  :prefix 'cef-prefix-window
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
  "i" #'string-rectangle
  "a" #'align-regexp
  "p" #'mark-paragraph
  "t" #'cef-transpose-paragraphs)

;; More Verbose Keybindings
(defvar-keymap cef-prefix-complex
  :doc "C-c C-c"
  :prefix 'cef-prefix-complex
  "e" #'emoji-insert
  "s" #'scratch-buffer
  "b" #'cef-browse-url-at-line
  "t" #'cef-open-terminal)

;; General Keymap For Custom User Bindings
(defvar-keymap cef-prefix
  :doc "Prefix ´C-c´ for General Bindings."
  :prefix 'cef-prefix
  "k" #'cef-kill-current-buffer
  "p" #'mark-paragraph
  "r" #'rectangle-mark-mode
  "i" #'string-rectangle
  ;; Dired
  "d" #'cef-open-dired-current-directory
  "h" #'cef-open-dired-home-directory
  ;; Global Font-Size Adjustments
  "+" #'global-text-scale-adjust
  "-" #'global-text-scale-adjust
  "0" #'cef-hard-reset-font-size
  ;; Switch Between Windows
  "o" #'other-window
  ;; File
  "f" #'find-file
  "F" #'find-file-other-window
  ;; --- Nested Keymaps ---
  "w" cef-prefix-window
  "e" cef-prefix-editing-commands
  "c" cef-prefix-complex)

;; Labeling of Nested Keymaps in which-key-mode
(if (featurep 'which-key)
    (which-key-add-keymap-based-replacements cef-prefix
      "w" `("Window/Buffer Management" . ,cef-prefix-window)
      "c" `("Complex"                  . ,cef-prefix-complex)
      "e" `("Text Editing"             . ,cef-prefix-editing-commands)))

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

;; Isearch Keybindings
(require 'isearch)
(defvar-keymap cef-isearch-mode
  :doc    "Custom Keybindings For Isearch Minor Mode."
  :keymap isearch-mode-map
  "C-n" #'isearch-repeat-forward
  "C-p" #'isearch-repeat-backward
  "C-s" nil
  "C-o" #'isearch-occur)

;; Dired Keybindings
(require 'dired)
(defvar-keymap cef-dired-mode
  :doc    "Custom Keybindings For Dired Major Mode."
  :keymap dired-mode-map
  "u"         #'cef-dired-directory-up
  "x"         #'cef-dired-toggle-execute-permission
  "m"         #'cef-dired-toggle-mark
  "<"         nil
  ">"         nil
  "^"         nil
  "t"         #'dired-toggle-marks
  "C-g"       #'dired-unmark-all-marks
  "<mouse-2>" #'dired-find-file)

;; => First Level Global Bindings
(defvar-keymap cef-global-mode
  :doc ""
  :parent nil
  ;; Overwrite All Keybindings except for
  "M-x" #'execute-extended-command
  "C-x" ctl-x-map
  ;; Repeat Last Run Command
  "C-." #'repeat
  ;; Toggle Comment On Line
  "C-," #'comment-line
  ;; Undo / Redo
  "C--" #'undo-redo
  "C-_" #'undo
  ;; Move Lines
  "M-p" #'cef-move-line-up
  "M-n" #'cef-move-line-down
  ;; Scrolling
  "C-v" #'cef-scroll-down
  "M-v" #'cef-scroll-up
  ;; Jump Paragraph
  "M-r" #'backward-paragraph
  "C-r" #'forward-paragraph
  ;; Delete
  "C-d" #'cef-delete-text
  ;; Global Font-Size Adjustments
  "C-x C-+" #'global-text-scale-adjust
  "C-x C--" #'global-text-scale-adjust
  "C-x C-0" #'cef-hard-reset-font-size
  ;; Nested Keybindings
  "C-c" cef-prefix)

;; --- Minor Mode ---
;; Idea: <https://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs>
(define-minor-mode cef-bindings
  "Minor Mode For Controlling Custom Keybindings."
  :init-value t
  :keymap cef-global-mode)

(cef-bindings t)

;; --- Export ---
(provide 'bindings)

;;; bindings.el ends here
