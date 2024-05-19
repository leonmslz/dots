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

;; --- Export ---
(provide 'languages)
