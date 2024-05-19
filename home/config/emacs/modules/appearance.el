;; -*- lexical-binding: t -*-
;; appearance.el - Theming And Appearance

;; Color-Scheme
(use-package ef-themes
  :ensure t
  :config
  (load-theme 'ef-elea-dark t))

;; Font-Face
(set-face-attribute 'default
                    nil
                    :family "Iosevka"
                    :height 105
                    :weight 'regular)

;; --- Export ---
(provide 'appearance)
