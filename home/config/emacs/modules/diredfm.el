;; diredfm.el --- Dired (Emacs Directory Editor) Related Settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; --- Settings ---

;; Only One Dired Buffer
(setq dired-kill-when-opening-new-dired-buffer t)

;; List Directories First
(setq dired-listing-switches "-alh --group-directories-first")

;; Hide Details
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; Recursive Delete And Copy
(setq dired-recursive-copies  'always)
(setq dired-recursive-deletes 'always)

;; --- Functions ---

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

;; Make Files Executable
(defun cef-dired-toggle-execute-permission ()
  "Toggle Execute-Permission Of All Selected Files."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (dolist (f files)
      (when-let* ((cmode (file-modes f))
                  (nmode (if (zerop (logand cmode #o111)) (logior cmode #o111) (logand cmode (lognot #o111)))))
        (set-file-modes f nmode)))))

;; Toggle Mark On File
(defun cef-dired-toggle-mark ()
  "Toggle Mark On The Current File."
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (beginning-of-line)
      (if (looking-at-p " ")
          (dired-mark 1)
        (dired-unmark 1)))
    (forward-line)))

;; --- Export ---
(provide 'diredfm)

;;; diredfm.el ends here
