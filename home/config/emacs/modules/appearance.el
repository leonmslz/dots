;; appearance.el --- Theming And Appearance -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(load-theme 'gruvbox t)

;; --- Tree-Sitter ---

(when (treesit-available-p)

  (use-package tree-sitter
    :ensure t
    :config
    (global-tree-sitter-mode t))

  (use-package tree-sitter-langs
    :ensure t)

  (add-hook 'prog-mode-hook #'tree-sitter-mode)
  (add-hook 'prog-mode-hook #'tree-sitter-hl-mode)
  )

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
                    :family "ZedMono Nerd Font"
                    :height cef-font-size
                    :weight 'medium)

;; --- Export ---
(provide 'appearance)

;;; appearance.el ends here
