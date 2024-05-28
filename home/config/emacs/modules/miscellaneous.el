;; -*- lexical-binding: t -*-
;; miscellaneous.el - Additional Custom Functions

;; --- Custom Functions ---

;; => Open A Terminal

;; Automaticly Kill Buffer When Exiting The Shell
;; <https://stackoverflow.com/questions/12624815/how-to-automatically-kill-buffer-on-terminal-process-exit-in-emacs>
(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  (kill-buffer))

(defun cef-open-terminal ()
  "Open A Terminal Buffer Using The Default User Shell."
  (interactive)
  (term (getenv "SHELL")))

;; => Open URL
(defun cef-browse-url-at-line ()
  "Extract A URL From The Current Line And Open It In The Default Web Browser."
  (interactive)
  (let*   ((bounds-of-line (bounds-of-thing-at-point 'line))
           (current-line   (buffer-substring-no-properties (car bounds-of-line) (cdr bounds-of-line)))
           (url-regex      "\\bhttps?://[[:alnum:]#%$&\\?()~_+=/\\.-]+\\b"))
    (if (string-match url-regex current-line)
        (progn
          (setq-local url-as-string (match-string 0 current-line))
          (message (format "Opening `%s` In The Default Web Browser." url-as-string))
          (browse-url url-as-string))
      (error "Unable To Extract URL From Current Line."))))

;; => Inserting Current Date
(defun cef-insert-todays-date ()
  "Insert The Current Date At Cursor Position."
  (interactive)
  (insert
   (format-time-string "%d.%m.%Y")))

;; => Moving Lines
;; Copied From <https://www.emacswiki.org/emacs/MoveLine>

(defmacro cef-save-column (&rest body)
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@body)
       (move-to-column column))))
(put 'cef-save-column 'lisp-indent-function 0)

(defun cef-move-line-up ()
  "Move The Current Line One Line Up."
  (interactive)
  (cef-save-column
    (transpose-lines 1)
    (forward-line -2)))

(defun cef-move-line-down ()
  "Move The Current Line One Line Down."
  (interactive)
  (cef-save-column
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)))

;; => Scroll Up/Down
;; In Order For This To Work The Variable recenter-postions Has To Be Set To Only middle.
;; (setq recenter-positions '(middle)) ;; See Basic Settings ^^

(defun cef-scroll-down ()
  "Scroll Up While Keeping The Cursor In The Center Of The Screen."
  (interactive)
  (scroll-up-command)
  (recenter-top-bottom))

(defun cef-scroll-up ()
  "Scroll Down While Keeping The Cursor In The Center Of The Screen."
  (interactive)
  (scroll-down-command)
  (recenter-top-bottom))

;; => Auto-create Missing Directories
;; Reference: <https://emacsredux.com/blog/2022/06/12/auto-create-missing-directories/>
(defun cef-auto-create-missing-dirs-find-file ()
  (let ((target-dir (file-name-directory buffer-file-name)))
    (unless (file-exists-p target-dir)
      (make-directory target-dir t))))

(add-to-list 'find-file-not-found-functions #'cef-auto-create-missing-dirs-find-file)

;; => Delete Text
(defun cef-delete-text ()
  "Delete Text Into Kill Ring If Selected, Otherwise Delete Character."
  (interactive)
  (if mark-active
      (delete-active-region t)
    (delete-char 1)))

;; => Dired Related Functions

;; Open Dired In Place
(defun cef-open-dired-current-directory ()
  "Open Dired In Place."
  (interactive)
  (dired "."))

;; Open Dired In Home Folder
(defun cef-open-dired-home-directory ()
  "Open Dired In Home Folder."
  (interactive)
  (dired "~"))

;; Go Up A Directory
(defun cef-dired-directory-up ()
  "Move Up A Directory."
  (interactive)
  (find-alternate-file ".."))

;; --- Export ---
(provide 'miscellaneous)
