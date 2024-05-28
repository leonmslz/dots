;; -*- lexical-binding: t -*-
;; mode-line.el - Custom Mode-Line Configuration

(defgroup cef-mode-line nil
  "Custom Minimalistic Mode-Line."
  :group 'cef-emacs-configuration)

;; Height Of Mode-Line
(defvar-local cef-mline-module/beginning
    '(:eval (propertize " " 'display '((height 1.5) (raise -0.15))))
  "Mode-Line Module Which Puts A Small Icon Depending On A State On The Mode-Line.")
(put 'cef-mline-module/beginning 'risky-local-variable t)

;; --- Faces ---

(defgroup cef-modeline-faces nil
  "Faces for my custom modeline."
  :group 'cef-emacs-configuration)

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

(defface cef-mline-face/orange-indicator
  '((default)
    (((class color) (min-colors 88) (background light))
     :foreground "orange")
    (((class color) (min-colors 88) (background dark))
     :foreground "#EBA8A8")
    (t :foreground "orange"))
  "Mode-Line Face For `cef-mline-module/buffer-name'."
  :group 'cef-modeline-faces)

(defface cef-mline-face/cyan-indicator
  '((default)
    (((class color) (min-colors 88) (background light))
     :foreground "cyan")
    (((class color) (min-colors 88) (background dark))
     :foreground "#99BFCF")
    (t :foreground "cyan"))
  "Mode-Line Face For `cef-mline-module/buffer-name'."
  :group 'cef-modeline-faces)

(defface cef-mline-face/yellow-indicator
  '((default)
    (((class color) (min-colors 88) (background light))
     :foreground "yellow")
    (((class color) (min-colors 88) (background dark))
     :foreground "#FFE36C")
    (t :foreground "yellow"))
  "Mode-Line Face For `cef-mline-module/buffer-name'."
  :group 'cef-modeline-faces)

(defface cef-mline-face/green-indicator
  '((default)
    (((class color) (min-colors 88) (background light))
     :foreground "green")
    (((class color) (min-colors 88) (background dark))
     :foreground "#50CF89")
    (t :foreground "green"))
  "Mode-Line Face For `cef-mline-module/buffer-name'."
  :group 'cef-modeline-faces)

(defface cef-mline-face/magenta-indicator
  '((default)
    (((class color) (min-colors 88) (background light))
     :foreground "magenta")
    (((class color) (min-colors 88) (background dark))
     :foreground "#BF9FEC")
    (t :foreground "magenta"))
  "Mode-Line Face For `cef-mline-module/buffer-name'."
  :group 'cef-modeline-faces)

(defface cef-mline-face/help-command
  '((t :foreground "grey"))
  "Mode-Line Face For `cef-mline-module/help-command'."
  :group 'cef-modeline-faces)

;; --- Modules ---

;; => Module: Emoji-Icon

(defun cef-mline-fun/get-icon ()
  "Returns A Formatted String Containing An Icon Depending On A State Of The Current Buffer."
  (let ((emoji-icon
         (cond ((eq major-mode 'dired-mode) "🗂️")
               ((eq major-mode 'term-mode)  "🖥️")
               ((eq major-mode 'help-mode)  "ℹ️️")
               ((eq major-mode 'Info-mode)  "ℹ️️")
               ((buffer-modified-p)         "📃️")
               (t                           "💾️"))))
    (propertize (format " %s " emoji-icon)
                'face (if (mode-line-window-selected-p)
                          'cef-mline-face/green-background-active
                        'cef-mline-face/green-background-inactive))))

(defvar-local cef-mline-module/emoji-icon
    '(:eval (cef-mline-fun/get-icon))
  "Mode-Line Module Which Puts A Small Icon Depending On A State Of The Current Buffer On The Mode-Line.")
(put 'cef-mline-module/emoji-icon 'risky-local-variable t)

;; => Module: Buffer-Name

(defun cef-mline-fun/buffer-name ()
  "Returns A Formatted String Containing The Current Buffer Name."
  (propertize (format " %s " (buffer-name))
              'face (if (mode-line-window-selected-p)
                        'cef-mline-face/white-foreground-active
                      'cef-mline-face/white-foreground-inactive)))

(defvar-local cef-mline-module/buffer-name
    '(:eval (cef-mline-fun/buffer-name))
  "Mode-Line Module Which Displays The Current Buffer Name.")
(put 'cef-mline-module/buffer-name 'risky-local-variable t)

;; => Module: Major-Mode

(defun cef-mline-fun/major-mode ()
  "Returns A Formatted String Containing The Major-Mode Of The Current Buffer."
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
  "Mode-Line Module Which Displays The Major-Mode Of The Current Buffer.")
(put 'cef-mline-module/major-mode 'risky-local-variable t)

;; => Module: Line-Number

(defun cef-mline-fun/count-lines-of-code ()
  "Count The Lines Of Code In The Current Buffer."
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
  "Returns A Formatted String Containing The Current Line-Number, Total Number Of Lines And Lines Of Code."
  (let ((cline "%l")
        (tline (number-to-string (count-lines (point-min) (point-max))))
        (loc   (number-to-string (cef-mline-fun/count-lines-of-code))))
    (format " %s %s/%s (%s) "
            (propertize ""  'face 'cef-mline-face/cyan-indicator)
            cline
            tline
            loc)))

(defvar-local cef-mline-module/line-number
    '(:eval (when (and (not (eq display-line-numbers nil))
                       (mode-line-window-selected-p))
              (cef-mline-fun/line-number)))
  "Mode-Line Module Which Displays The Current Line-Number, Total Number Of Lines And Lines Of Code.")
(put 'cef-mline-module/line-number 'risky-local-variable t)

;; => Module: Dired-Pwd

(defun cef-shorten-path (path max-len)
  "Shorten PATH to a maximum length of MAX-LEN by removing middle elements."
  (let* ((cur-len   (length path))
         (elems     (split-string path "/"))
         (elem-cons (mapcar (lambda (x) (cons x (length x))) elems)))
    (if (<= cur-len max-len)
        path
      (while-let ((_ (> cur-len max-len)) ;; Main-Condition
                  ;; Additional Variables
                  (len-el-w-l (length elem-cons))
                  (mid-val    (if (= len-el-w-l 1) 0 (1- (/ len-el-w-l 2))))
                  (cdr-sum    0))
        ;; Remove Middle-Element From List
        (setq elem-cons (append (seq-take elem-cons mid-val) (seq-drop elem-cons (1+ mid-val))))
        ;; Add Up Length Of All Remaining Path-Parts + Slashes
        (setq cur-len   (+ (dolist (cell elem-cons cdr-sum) (setq cdr-sum (+ cdr-sum (cdr cell))))
                           (1- len-el-w-l) 3))
        (setq out mid-val)) ;; Export Mid-Value
      ;; Collect All Elements As A Path-String
      (let* ((mid-val out)
             (el      (mapcar #'car elem-cons)))
        (mapconcat 'identity (append (seq-take el mid-val) (list "...") (seq-drop el mid-val)) "/")))))

(defun cef-mline-fun/dired-pwd ()
  "Returns A Formatted String Containing PWD Of Current Dired Buffer."
  (let* ((pwd  (expand-file-name default-directory))
         (path (replace-regexp-in-string (getenv "HOME") "~" pwd)))
    (format " %s %s "
            (propertize "" 'face 'cef-mline-face/cyan-indicator)
            (cef-shorten-path path 32))))

(defvar-local cef-mline-module/dired-pwd
    '(:eval (when (and (eq major-mode 'dired-mode)
                       (mode-line-window-selected-p))
              (cef-mline-fun/dired-pwd)))
  "Mode-Line Module Which Displays The Current PWD In Dired-Major-Mode.")
(put 'cef-mline-module/dired-pwd 'risky-local-variable t)

;; => Module: Time/Date-String

(defun cef-mline-fun/time-date-string ()
  "Returns A Formatted String Containing Current Time And Date."
  (format " %s %s "
          (propertize "" 'face 'cef-mline-face/cyan-indicator)
          (format-time-string "%H:%M, %d. %b %Y")))

(defvar-local cef-mline-module/time-date-string
    '(:eval (when (mode-line-window-selected-p)
              (cef-mline-fun/time-date-string)))
  "Mode-Line Module Which Displays The Current Time And Date.")
(put 'cef-mline-module/time-date-string 'risky-local-variable t)

;; => Module: Shell

(defun cef-mline-fun/shell ()
  "Returns A Formatted String Containing 'Shell'-Env-Var."
  (let* ((path  (getenv "SHELL"))
         (shell (file-name-nondirectory (directory-file-name path))))
    (format " %s %s "
            (propertize "" 'face 'cef-mline-face/cyan-indicator)
            shell)))

(defvar-local cef-mline-module/shell
    '(:eval (when (and (eq major-mode 'term-mode)
                       (mode-line-window-selected-p))
              (cef-mline-fun/shell)))
  "Mode-Line Module Which Displays The Value Of 'Shell'-Env-Var In Term-Major-Mode.")
(put 'cef-mline-module/shell 'risky-local-variable t)

;; => Module: Help-Command

(defun cef-mline-fun/help-command ()
  "Get the first word from the current buffer, treating words by space."
  (let ((command (save-excursion
                   (goto-char (point-min))
                   (if (re-search-forward "\\S-" nil t)
                       (buffer-substring-no-properties (match-beginning 0) (progn (skip-syntax-forward "^ ") (point)))
                     ""))))
    (propertize (format "{ %s}" command)
                'face 'cef-mline-face/help-command)))

(defvar-local cef-mline-module/help-command
    '(:eval (when (and (eq major-mode 'help-mode)
                       (mode-line-window-selected-p))
              (cef-mline-fun/help-command)))
  "Mode-Line Module Which Displays The Current Buffer Name.")
(put 'cef-mline-module/help-command 'risky-local-variable t)

;; => Module: Git-Branch

(defun get-git-branch ()
  "Get The Current Git-Branch."
  (unless (featurep 'vc-git)
    (require 'vc-git))
  (vc-git--symbolic-ref (expand-file-name default-directory)))

(defun cef-mline-fun/git-branch (branch)
  "Returns A Formatted String Containing The Current Git-Branch."
  (format " %s %s "
          (propertize "" 'face 'cef-mline-face/cyan-indicator)
          branch))

(defvar-local cef-mline-module/git-branch
    '(:eval (when-let ((_ (mode-line-window-selected-p))
                       (_ (or (bound-and-true-p vc-mode) (eq major-mode 'dired-mode)))
                       (branch (get-git-branch)))
              (cef-mline-fun/git-branch branch)))
  "Mode-Line Module Which Displays The Git-Branch Of A Git Tracked File Or Directory.")
(put 'cef-mline-module/git-branch 'risky-local-variable t)

;; --- Mode-Line-Construction ---

;; => Mode-Line Renderer
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
                  cef-mline-module/git-branch
                  cef-mline-module/dired-pwd
                  cef-mline-module/shell
                  cef-mline-module/help-command
                  cef-mline-module/line-number
                  cef-mline-module/time-date-string
                  ))
          ))))

(setq-default mode-line-format mode-line-format)

;; --- Export ---
(provide 'mode-line)
