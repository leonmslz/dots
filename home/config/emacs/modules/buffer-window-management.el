;; -*- lexical-binding: t -*-
;; buffer-window-management.el - Buffer/Window Related Settings And Functions

;; Balanced Windows <https://zck.org/balance-emacs-windows>
(setq window-combination-resize t)

;; Enable Winner-Mode
(winner-mode t)

;; Kill Current Buffer
(defun cef-kill-current-buffer ()
  "Kill The Current Buffer."
  (interactive)
  (kill-buffer (buffer-name)))

;; => Switching User Buffers
;; Idea And Code Partially Stolen From <http://xahlee.info/emacs/emacs/elisp_next_prev_user_buffer.html>

(defmacro cef-cycle-through-user-buffers (next-or-prev)
  "Macro For Cycling Through User Buffers."
  `(let (start-in-buffer
         end-in-buffer)
     (setq start-in-buffer (buffer-name))
     (,next-or-prev)
     (let ((i 0))
       (while (< i 20)
         (if (string-equal "*" (substring (buffer-name) 0 1))
             (progn (,next-or-prev)
                    (setq i (+ i 1)))
           (setq i 100
                 end-in-buffer (buffer-name)))))
     (when (eq start-in-buffer end-in-buffer)
       (error "There Is Only One User Buffer Available!"))))

(defun cef-switch-to-next-user-buffer ()
  "Switch To The Next User Buffer."
  (interactive)
  (cef-cycle-through-user-buffers next-buffer))

(defun cef-switch-to-previous-user-buffer ()
  "Switch To The Previous User Buffer."
  (interactive)
  (cef-cycle-through-user-buffers previous-buffer))

;; => Buffer Movement
;; Idea And Code Partially Stolen From <https://github.com/lukhas/buffer-move>

(require 'windmove)

(defun cef-move-window (direction)
  "Helper Function To Move A Window In The Speciefied Direction."

  (let (other-window-p
        buffer-this-buffer)

    (setq other-window-p     (windmove-find-other-window direction)
          buffer-this-buffer (window-buffer (selected-window)))

    (when (or (null other-window-p)
              (and (eq direction 'down)
                   (string-match "^ \\*Minibuf" (buffer-name (window-buffer other-window-p)))))
      (error "Unable To Swap Buffers."))

    (set-window-buffer (selected-window) (window-buffer other-window-p))

    (set-window-buffer other-window-p buffer-this-buffer)
    (select-window other-window-p)))

(defun cef-move-window-up ()
  "Move The Current Window Up."
  (interactive)
  (cef-move-window 'up))

(defun cef-move-window-down ()
  "Move The Current Window Down."
  (interactive)
  (cef-move-window 'down))

(defun cef-move-window-left ()
  "Move The Current Window To The Left."
  (interactive)
  (cef-move-window 'left))

(defun cef-move-window-right ()
  "Move The Current Window To The Right."
  (interactive)
  (cef-move-window 'right))

;; => Split Window

(defun cef-split-window-on-vertical-axis ()
  "Split The Current Window On The Vertical Axis."
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(defun cef-split-window-on-horizontal-axis ()
  "Split The Current Window On The Horizontal Axis."
  (interactive)
  (split-window-vertically)
  (other-window 1))

;; --- Export ---
(provide 'buffer-window-management)
