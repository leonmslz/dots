;; gruvbox-theme.el --- Gruvbox Theme For Emacs -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(deftheme gruvbox
  "Gruvbox Color Theme For Emacs.")

(let ((class '((class color) (min-colors 89)))

      ;; Gruvbox Material palette
      (cef-dark0         "#242424")
      (cef-dark1         "#3c3836")
      (cef-dark2         "#45403d")
      (cef-dark3         "#5a524c")
      (cef-dark4         "#7c6f64")

      (cef-light0        "#ddc7a1")
      (cef-light1        "#d4be98")
      (cef-light2        "#c5b18d")
      (cef-light3        "#a89984")
      (cef-light4        "#928374")

      (cef-bright-red    "#ea6962")
      (cef-bright-green  "#a9b665")
      (cef-bright-yellow "#d8a657")
      (cef-bright-blue   "#7daea3")
      (cef-bright-purple "#d3869b")
      (cef-bright-aqua   "#89b482")
      (cef-bright-orange "#e78a4e"))

  (custom-theme-set-faces
   'gruvbox
   ;; Basic colors
   `(default                           ((,class (:background ,cef-dark0 :foreground ,cef-light0))))
   `(cursor                            ((,class (:background ,cef-light0))))
   `(fringe                            ((,class (:background ,cef-dark0))))
   `(region                            ((,class (:background ,cef-dark2))))
   `(highlight                         ((,class (:background ,cef-dark1))))
   `(hl-line                           ((,class (:background ,cef-dark1))))
   `(vertical-border                   ((,class (:foreground ,cef-dark2))))
   `(minibuffer-prompt                 ((,class (:foreground ,cef-bright-blue :bold t))))
   `(link                              ((,class (:foreground ,cef-bright-blue :underline t))))
   `(warning                           ((,class (:foreground ,cef-bright-orange :bold t))))

   ;; Line Numbers
   `(line-number                       ((,class (:foreground ,cef-dark4))))
   `(line-number-current-line          ((,class (:foreground ,cef-bright-orange :background ,cef-dark1 :bold t))))
   `(line-number-major-tick            ((,class (:foreground ,cef-bright-yellow))))

   ;; Term Mode
   `(term                              ((,class (:foreground ,cef-light0 :background ,cef-dark0))))
   `(term-color-black                  ((,class (:foreground ,cef-dark0 :background ,cef-dark1))))
   `(term-color-red                    ((,class (:foreground ,cef-bright-red :background ,cef-dark0))))
   `(term-color-green                  ((,class (:foreground ,cef-bright-green :background ,cef-dark0))))
   `(term-color-yellow                 ((,class (:foreground ,cef-bright-yellow :background ,cef-dark0))))
   `(term-color-blue                   ((,class (:foreground ,cef-bright-blue :background ,cef-dark0))))
   `(term-color-magenta                ((,class (:foreground ,cef-bright-purple :background ,cef-dark0))))
   `(term-color-cyan                   ((,class (:foreground ,cef-bright-aqua :background ,cef-dark0))))
   `(term-color-white                  ((,class (:foreground ,cef-light0 :background ,cef-dark0))))
   `(term-color-bright-black           ((,class (:foreground ,cef-dark4 :background ,cef-dark0))))
   `(term-color-bright-red             ((,class (:foreground ,cef-bright-red :background ,cef-dark0))))
   `(term-color-bright-green           ((,class (:foreground ,cef-bright-green :background ,cef-dark0))))
   `(term-color-bright-yellow          ((,class (:foreground ,cef-bright-yellow :background ,cef-dark0))))
   `(term-color-bright-blue            ((,class (:foreground ,cef-bright-blue :background ,cef-dark0))))
   `(term-color-bright-magenta         ((,class (:foreground ,cef-bright-purple :background ,cef-dark0))))
   `(term-color-bright-cyan            ((,class (:foreground ,cef-bright-aqua :background ,cef-dark0))))
   `(term-color-bright-white           ((,class (:foreground ,cef-light1 :background ,cef-dark0))))

   ;; Mode line faces
   `(mode-line                         ((,class (:foreground ,cef-light0 :background ,cef-dark2))))
   `(mode-line-inactive                ((,class (:foreground ,cef-light4 :background ,cef-dark1))))

   ;; Flyspell faces
   `(flyspell-duplicate                ((,class (:underline (:color ,cef-bright-yellow)))))
   `(flyspell-incorrect                ((,class (:underline (:color ,cef-bright-red)))))

   ;; Flycheck faces
   `(flycheck-error                    ((,class (:underline (:color ,cef-bright-red)))))
   `(flycheck-warning                  ((,class (:underline (:color ,cef-bright-yellow)))))
   `(flycheck-info                     ((,class (:underline (:color ,cef-bright-blue)))))
   `(flycheck-fringe-error             ((,class (:foreground ,cef-bright-red :background ,cef-dark0))))
   `(flycheck-fringe-warning           ((,class (:foreground ,cef-bright-yellow :background ,cef-dark0))))
   `(flycheck-fringe-info              ((,class (:foreground ,cef-bright-blue :background ,cef-dark0))))

   ;; Isearch faces
   `(isearch                           ((,class (:foreground ,cef-dark0 :background ,cef-bright-yellow :weight bold))))
   `(isearch-fail                      ((,class (:foreground ,cef-dark0 :background ,cef-bright-red :weight bold))))
   `(isearch-lazy-highlight            ((,class (:foreground ,cef-light0 :background ,cef-dark2))))
   `(isearch-persistent                ((,class (:foreground ,cef-light0 :background ,cef-dark2))))
   `(lazy-highlight                    ((,class (:foreground ,cef-light0 :background ,cef-dark2))))

   ;; Org-mode faces
   `(org-level-1                       ((,class (:foreground ,cef-bright-red :weight bold))))
   `(org-level-2                       ((,class (:foreground ,cef-bright-orange :weight bold))))
   `(org-level-3                       ((,class (:foreground ,cef-bright-yellow :weight bold))))
   `(org-level-4                       ((,class (:foreground ,cef-bright-green :weight bold))))
   `(org-level-5                       ((,class (:foreground ,cef-bright-blue :weight bold))))
   `(org-level-6                       ((,class (:foreground ,cef-bright-purple :weight bold))))
   `(org-level-7                       ((,class (:foreground ,cef-bright-aqua :weight bold))))
   `(org-level-8                       ((,class (:foreground ,cef-light0 :weight bold))))
   `(org-link                          ((,class (:foreground ,cef-bright-blue :underline t))))
   `(org-todo                          ((,class (:foreground ,cef-bright-red :weight bold :background ,cef-dark0))))
   `(org-done                          ((,class (:foreground ,cef-bright-green :weight bold :background ,cef-dark0))))
   `(org-block                         ((,class (:foreground ,cef-light0 :background ,cef-dark1 :extend t))))
   `(org-code                          ((,class (:foreground ,cef-bright-yellow :background ,cef-dark0))))
   `(org-verbatim                      ((,class (:foreground ,cef-bright-orange :background ,cef-dark0))))
   `(org-quote                         ((,class (:foreground ,cef-light3 :slant italic :background ,cef-dark1))))
   `(org-date                          ((,class (:foreground ,cef-bright-blue :underline t))))
   `(org-checkbox                      ((,class (:foreground ,cef-bright-yellow :weight bold))))
   `(org-table                         ((,class (:foreground ,cef-bright-blue :background ,cef-dark1))))
   `(org-special-keyword               ((,class (:foreground ,cef-bright-aqua :weight bold))))
   `(org-block-begin-line              ((,class (:foreground ,cef-dark4 :background ,cef-dark0 :extend t))))
   `(org-block-end-line                ((,class (:foreground ,cef-dark4 :background ,cef-dark0 :extend t))))

   ;; Rainbow delimiters faces
   `(rainbow-delimiters-depth-1-face   ((,class (:foreground ,cef-bright-red))))
   `(rainbow-delimiters-depth-2-face   ((,class (:foreground ,cef-bright-orange))))
   `(rainbow-delimiters-depth-3-face   ((,class (:foreground ,cef-bright-yellow))))
   `(rainbow-delimiters-depth-4-face   ((,class (:foreground ,cef-bright-green))))
   `(rainbow-delimiters-depth-5-face   ((,class (:foreground ,cef-bright-blue))))
   `(rainbow-delimiters-depth-6-face   ((,class (:foreground ,cef-bright-purple))))
   `(rainbow-delimiters-depth-7-face   ((,class (:foreground ,cef-bright-aqua))))
   `(rainbow-delimiters-depth-8-face   ((,class (:foreground ,cef-light0))))
   `(rainbow-delimiters-depth-9-face   ((,class (:foreground ,cef-dark4))))
   `(rainbow-delimiters-unmatched-face ((,class (:foreground ,cef-bright-red :background ,cef-dark0 :weight bold))))

   ;; Font lock faces
   `(font-lock-builtin-face            ((,class (:foreground ,cef-bright-orange))))
   `(font-lock-comment-face            ((,class (:foreground ,cef-dark4))))
   `(font-lock-constant-face           ((,class (:foreground ,cef-bright-purple))))
   `(font-lock-function-name-face      ((,class (:foreground ,cef-bright-yellow))))
   `(font-lock-keyword-face            ((,class (:foreground ,cef-bright-red))))
   `(font-lock-string-face             ((,class (:foreground ,cef-bright-green))))
   `(font-lock-type-face               ((,class (:foreground ,cef-bright-purple))))
   `(font-lock-variable-name-face      ((,class (:foreground ,cef-bright-blue))))
   `(font-lock-warning-face            ((,class (:foreground ,cef-bright-red :bold t))))
   ))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; --- Export ---
(provide-theme 'gruvbox)

;;; gruvbox-theme.el ends here
