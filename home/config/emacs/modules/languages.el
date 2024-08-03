;; -*- lexical-binding: t -*-
;; languages.el - Support For Programming Languages

;; List With Languages To Install
(setq cef-languages
      '(go-mode
        lua-mode
        yaml-mode
        markdown-mode
        rust-mode
        csharp-mode
        dockerfile-mode
        haskell-mode
        nix-mode
        clojure-mode
        python-mode
        git-modes
        ))

;; Install All Language-Modes
(dolist (language cef-languages)
  (unless (package-installed-p language)
    (package-install language)))

;; => Add Certain File-Extensions To Modes
(add-to-list 'auto-mode-alist '("\\.pde\\'" . java-mode))

;; Flycheck
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

;; --- Export ---
(provide 'languages)

;;; languages.el ends here
