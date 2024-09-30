;; early-init.el --- Customizations That Take Effect During Emacs Startup -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Emacs Default Frame Size
(when window-system
  (set-frame-size (selected-frame) 160 55))

;; Disable Unnecessary UI-Elements
(menu-bar-mode   -1)
(tool-bar-mode   -1)
(scroll-bar-mode -1)
(tooltip-mode    -1)

;; => Emacs Startup Time
;; Display Emacs Startup Time On Startup :D <https://github.com/daviwil/dotfiles>

;; The Default Is 800 Kilobytes. Measured In Bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile Emacs Startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

;;; early-init.el ends here
