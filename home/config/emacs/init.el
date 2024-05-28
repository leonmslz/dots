;; -*- lexical-binding: t -*-
;; init.el - Configuration File For GNU Emacs By Leon Schulz

;; Add Modules-Directory to Load-Path
(add-to-list 'load-path
             (expand-file-name "modules/" user-emacs-directory))

;; Import Modules
(require 'package-management)
(require 'basics)
(require 'appearance)
(require 'mode-line)
(require 'buffer-management)
(require 'miscellaneous)
(require 'interface)
(require 'languages)
(require 'bindings)
