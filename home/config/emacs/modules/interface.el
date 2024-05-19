;; -*- lexical-binding: t -*-
;; interface.el

;; => All the Icons

(use-package all-the-icons-dired
  :ensure t)

;; => Auto Complete

(use-package auto-complete
  :ensure t
  :init
  (global-auto-complete-mode))

(use-package rainbow-mode
  :ensure t)

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
  :bind (
         ;; ("C-u"     . consult-buffer)
         ("C-x b"   . consult-buffer)
         ("C-c m b" . consult-buffer)
         ("C-c m t" . consult-theme)
         ("C-c m f" . consult-line)
         ("C-c m y" . consult-yank-pop)))

;; --- Export ---
(provide 'interface)
