;; package-management.el --- Package Management -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(require 'package)

;; => MELPA Repositories
;; Milkypostman’s Emacs Lisp Package Archive <https://melpa.org/>
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; => Use-Package
;; Macro For Simplyfiying And Isolating Package Configurations <https://github.com/jwiegley/use-package>
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; => Auto-Package-Update
;; Automatically Update Emacs Packages
(use-package auto-package-update
  :ensure t
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

;; --- Export ---
(provide 'package-management)

;;; package-management.el ends here
