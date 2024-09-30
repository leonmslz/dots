;;; init.el --- Configuration File For GNU Emacs -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Add Modules-Directory to Load-Path
(add-to-list 'load-path
             (expand-file-name "modules/" user-emacs-directory))

(add-to-list 'custom-theme-load-path
             (expand-file-name "modules/" user-emacs-directory))

;; Import Modules
(require 'package-management)
(require 'basics)
(require 'interface)
(require 'appearance)
(require 'mode-line)
(require 'buffer-management)
(require 'miscellaneous)
(require 'languages)
(require 'diredfm)
(require 'bindings)

;;; init.el ends here
