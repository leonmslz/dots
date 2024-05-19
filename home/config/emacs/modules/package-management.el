;; -*- lexical-binding: t -*-
;; package-management.el - Package Management

;; => MELPA Repositories
;; Milkypostman’s Emacs Lisp Package Archive <https://melpa.org/>

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; => Use-Package
;; Macro For Simplyfiying And Isolating Package Configurations <https://github.com/jwiegley/use-package>

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; --- Export ---
(provide 'package-management)
