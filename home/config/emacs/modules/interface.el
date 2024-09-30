;; interface.el --- Interface Related Settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; --- Packages ---

(use-package auto-complete
  :ensure t
  :init
  (global-auto-complete-mode))

(use-package all-the-icons-dired
  :ensure t
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package rainbow-mode
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :hook
  (emacs-lisp-mode . rainbow-delimiters-mode))

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
  (marginalia-align 'center)
  :init
  (marginalia-mode))

(setq completion-styles '(basic substring partial-completion flex))

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

;;; interface.el ends here
