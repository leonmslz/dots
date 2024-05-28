;; -*- lexical-binding: t -*-
;; appearance.el - Theming And Appearance

;; Color-Scheme
(use-package ef-themes
  :ensure t
  :config
  (load-theme 'ef-elea-dark t))

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
