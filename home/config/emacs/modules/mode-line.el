;; :==:> Mode-Line
(defgroup cef-mode-line nil
  "Custom Minimalistic Mode-Line."
  :group 'cef-emacs-configuration)

;; Height Of Mode-Line

(defun cef-mline-fun/set-mline-height (h r)
  (propertize " " 'display `((height ,h) (raise ,r))))

(defvar-local cef-mline-module/beginning
    '(:eval (cef-mline-fun/set-mline-height 1.5 -0.15))
  "Mode-Line Module Which Puts A Small Icon Depending On A State On The Mode-Line.")
(put 'cef-mline-module/beginning 'risky-local-variable t)

;; --- Faces ---

(defgroup cef-modeline-faces nil
  "Faces for my custom modeline."
  :group 'cef-emacs-configuration)

;; Faces
;; Code Block Format Stolen From: <https://github.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/prot-lisp/prot-modeline.el>
(defface cef-mline-face/green-background-active
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#880000" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#a4c3b2" :foreground "white")
    (t :background "green" :foreground "white"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group 'cef-modeline-faces)

(defface cef-mline-face/green-background-inactive
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#880000" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#46574e" :foreground "white")
    (t :background "green" :foreground "white"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group 'cef-modeline-faces)

(defface cef-mline-face/white-foreground-active
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :foreground "white")
    (t :foreground "white"))
  "Mode-Line Face For `cef-mline-module/buffer-name'."
  :group 'cef-modeline-faces)

(defface cef-mline-face/white-foreground-inactive
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#949494")
    (((class color) (min-colors 88) (background dark))
     :foreground "#949494")
    (t :foreground "white"))
  "Mode-Line Face For `cef-mline-module/buffer-name'."
  :group 'cef-modeline-faces)

(defface cef-mline-face/grey-foreground-active
  '((default)
    (((class color) (min-colors 88) (background light))
     :foreground "grey")
    (((class color) (min-colors 88) (background dark))
     :foreground "grey")
    (t :foreground "grey"))
  "Mode-Line Face For `cef-mline-module/buffer-name'."
  :group 'cef-modeline-faces)

(defface cef-mline-face/grey-foreground-inactive
  '((default)
    (((class color) (min-colors 88) (background light))
     :foreground "#949494")
    (((class color) (min-colors 88) (background dark))
     :foreground "#949494")
    (t :foreground "white"))
  "Mode-Line Face For `cef-mline-module/buffer-name'."
  :group 'cef-modeline-faces)

(defface cef-mline-face/line-number
  '((t :foreground "grey"))
  "Mode-Line Face For `cef-mline-module/line-number'."
  :group 'cef-modeline-faces)

(defface cef-mline-face/dired-pwd
  '((t :foreground "grey"))
  "Mode-Line Face For `cef-mline-module/dired-pwd'."
  :group 'cef-modeline-faces)

(defface cef-mline-face/shell
  '((t :foreground "grey"))
  "Mode-Line Face For `cef-mline-module/shell'."
  :group 'cef-modeline-faces)

(defface cef-mline-face/help-command
  '((t :foreground "grey"))
  "Mode-Line Face For `cef-mline-module/help-command'."
  :group 'cef-modeline-faces)

;; Module: Emoji-Icon

(defun cef-mline-fun/get-icon ()
  ""
  (let ((emoji-icon
         (cond ((eq major-mode 'dired-mode) "🗂️")
               ((eq major-mode 'term-mode)  "🖥️")
               ((eq major-mode 'help-mode)  "ℹ️️")
               ((buffer-modified-p)         "📃️")
               (t                           "💾️"))))
    (propertize (format " %s " emoji-icon)
                'face (if (mode-line-window-selected-p)
                          'cef-mline-face/green-background-active
                        'cef-mline-face/green-background-inactive))))

(defvar-local cef-mline-module/emoji-icon
    '(:eval (cef-mline-fun/get-icon))
  "Mode-Line Module Which Puts A Small Icon Depending On A State On The Mode-Line.")
(put 'cef-mline-module/emoji-icon 'risky-local-variable t)

;; Module: Buffer-Name

(defun cef-mline-fun/buffer-name ()
  ""
  (propertize (format " %s " (buffer-name))
              'face (if (mode-line-window-selected-p)
                        'cef-mline-face/white-foreground-active
                      'cef-mline-face/white-foreground-inactive)))

(defvar-local cef-mline-module/buffer-name
    '(:eval (cef-mline-fun/buffer-name))
  "Mode-Line Module Which Displays The Current Buffer Name.")
(put 'cef-mline-module/buffer-name 'risky-local-variable t)

;; Module: Major-Mode

(defun cef-mline-fun/major-mode ()
  ""
  (let* ((mode-str (symbol-name major-mode))
         (str      (if (string-match "\\(.*\\)-mode$" mode-str)
                       (match-string 1 mode-str)
                     mode-str))
         (fmt      (upcase-initials str)))
    (propertize (format "(%s)" fmt)
                'face (if (mode-line-window-selected-p)
                          'cef-mline-face/grey-foreground-active
                        'cef-mline-face/grey-foreground-inactive))))

(defvar-local cef-mline-module/major-mode
    '(:eval (cef-mline-fun/major-mode))
  "Mode-Line Module Which Displays The Current Buffer Name.")
(put 'cef-mline-module/major-mode 'risky-local-variable t)

;; ---

;; Module: Line-Number

(defun cef-mline-fun/count-lines-of-code ()
  "Count the lines of code in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((lines 0))
      (while (not (eobp))
        (when (not (or (looking-at "\\s-*$")
                       (looking-at "\\s-*\\'")))
          (setq lines (1+ lines)))
        (forward-line 1))
      lines)))

(defun cef-mline-fun/line-number ()
  ""
  (let ((cline "%l")
        (tline (number-to-string (count-lines (point-min) (point-max))))
        (loc   (number-to-string (cef-mline-fun/count-lines-of-code))))
    (propertize (format "[ %s/%s (%s loc)]" cline tline loc)
                'face 'cef-mline-face/line-number)))

(defvar-local cef-mline-module/line-number
    '(:eval (when (and (not (eq display-line-numbers nil))
                       (mode-line-window-selected-p))
              (cef-mline-fun/line-number)))
  "Mode-Line Module Which Displays The Current Buffer Name.")
(put 'cef-mline-module/line-number 'risky-local-variable t)

;; Module: Dired-Pwd

(defun cef-mline-fun/dired-pwd ()
  ""
  (let* ((pwd  (expand-file-name default-directory))
         (path (replace-regexp-in-string (getenv "HOME") "~" pwd)))
    (propertize (format "[ %s]" path)
                'face 'cef-mline-face/dired-pwd)))

(defvar-local cef-mline-module/dired-pwd
    '(:eval (when (and (eq major-mode 'dired-mode)
                       (mode-line-window-selected-p))
              (cef-mline-fun/dired-pwd)))
  "Mode-Line Module Which Displays The Current Buffer Name.")
(put 'cef-mline-module/dired-pwd 'risky-local-variable t)

;; Module: Shell

(defun cef-mline-fun/shell ()
  ""
  (let* ((path  (getenv "SHELL"))
         (shell (file-name-nondirectory (directory-file-name path)))
         (fmt   (upcase-initials shell)))
    (propertize (format "[ %s]" fmt)
                'face 'cef-mline-face/shell)))

(defvar-local cef-mline-module/shell
    '(:eval (when (and (eq major-mode 'term-mode)
                       (mode-line-window-selected-p))
              (cef-mline-fun/shell)))
  "Mode-Line Module Which Displays The Current Buffer Name.")
(put 'cef-mline-module/shell 'risky-local-variable t)

;; Module: Help-Command

(defun cef-mline-fun/help-command ()
  "Get the first word from the current buffer, treating words by space."
  (let ((command (save-excursion
                   (goto-char (point-min))
                   (if (re-search-forward "\\S-" nil t)
                       (buffer-substring-no-properties (match-beginning 0) (progn (skip-syntax-forward "^ ") (point)))
                     ""))))
    (propertize (format "[ %s]" command)
                'face 'cef-mline-face/help-command)))

(defvar-local cef-mline-module/help-command
    '(:eval (when (and (eq major-mode 'help-mode)
                       (mode-line-window-selected-p))
              (cef-mline-fun/help-command)))
  "Mode-Line Module Which Displays The Current Buffer Name.")
(put 'cef-mline-module/help-command 'risky-local-variable t)

;; Module: Git-Branch

(defun cef-mline-fun/git-branch ()
  "Get the current Git branch."
  (let ((branch (replace-regexp-in-string "[[:space:]]+" "" (substring-no-properties vc-mode))))
    (propertize (format "[ %s]" branch)
                'face 'cef-mline-face/line-number)))

(defvar-local cef-mline-module/git-branch
    '(:eval (when (and (bound-and-true-p vc-mode)
                       (mode-line-window-selected-p))
              (cef-mline-fun/git-branch)))
  "Mode-Line Module Which Displays The Current Buffer Name.")
(put 'cef-mline-module/git-branch 'risky-local-variable t)

;; Mode-Line Renderer
;; Source: <https://emacs.stackexchange.com/questions/5529/how-to-right-align-some-items-in-the-modeline>

(defun mode-line-render (left right)
  "Return a string of `window-width' length.
   Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width
         (- (window-total-width)
            (+ (length (format-mode-line left))
               (length (format-mode-line right))
               3))))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))

;; Construct Mode-Line

(setq mode-line-format
      '((:eval
         (mode-line-render
          ;; Left Modules
          (quote ("%e"
                  cef-mline-module/beginning
                  cef-mline-module/emoji-icon
                  cef-mline-module/buffer-name
                  cef-mline-module/major-mode
                  ))
          ;; Right Modules
          (quote ("%e"
                  cef-mline-module/dired-pwd
                  cef-mline-module/shell
                  cef-mline-module/git-branch
                  cef-mline-module/help-command
                  cef-mline-module/line-number
                  ))
          ))))

(setq-default mode-line-format mode-line-format)

(provide 'mode-line)
