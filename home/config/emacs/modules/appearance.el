;; -*- lexical-binding: t -*-
;; appearance.el - Theming And Appearance

;; Color-Scheme
(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-medium t))

;; => Font-Face

;; Font-Size
(defvar cef-font-size 105)

(defun cef-hard-reset-font-size ()
  "Reset Font Size To `cef-font-size'."
  (interactive)
  (set-face-attribute 'default
                    nil
                    :height cef-font-size))

;; Apply Face-Attributes
(set-face-attribute 'default
                    nil
                    :family "Iosevka"
                    :height cef-font-size
                    :weight 'regular)

;; --- Export ---
(provide 'appearance)
