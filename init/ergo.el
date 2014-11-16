;;; ergo.el --- UX initialization module -*- lexical-binding: t -*-

;;; Commentary:
;;
(require 'use-package)
;;; Code:

(global-set-key (kbd "C-s-t") (lambda () (interactive) (multi-term)))
(global-set-key (kbd "C-s-s") 'speedbar)

(require 'uniquify)
(when window-system
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))
(ido-mode 1)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(show-paren-mode 1)

(column-number-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; In order to make dead-circumflex work on emacs 24
(when (>= emacs-major-version 24)
  (require 'iso-transl))

(defvar my-confdir)

(global-set-key (kbd "C-z") 'undo)

;; Keyboard

(defun my-switch-ctrl-caps-lock ()
  "Switch ctrl and caps lock keys on keyboard."
  (interactive)
  (shell-command (concat "xmodmap " my-confdir "xmodmap")))

;; Copy file name

(defun my-kill-file-name ()
  "Insert the buffer's file name into kill ring."
  (interactive)
  (kill-new (buffer-file-name)))


;; Multiple cursors

(use-package multiple-cursors
  :ensure multiple-cursors
  :init (progn
          (global-set-key (kbd "C-<kp-2>") 'mc/mark-next-like-this)
          (global-set-key (kbd "C-<kp-8>") 'mc/mark-previous-like-this)
          (global-set-key (kbd "C-<kp-5>") 'mc/mark-all-dwim)))

;; Alignment

(defun my-align-detect-pattern (pattern)
  "Return the point of PATTERN on the current line.

Return nil if it could not find it."
  (save-excursion
    (let ((line-start (line-beginning-position))
          (line-end (line-end-position))
          match-point)
      (goto-char line-start)
      (setq match-point (search-forward pattern nil t))
      (if (and match-point
               (<= match-point line-end))
          (match-beginning 0)
        nil))))

(defun my-align-apply-to-neighbours (pattern callback)
  "Walk on all the neighbour lines that have PATTERN.

Apply CALLBACK at each line.  Point is placed to matching position before
calling CALLBACK.
Walk from, up to down."
  (save-excursion
    (while (my-align-detect-pattern pattern)
      (forward-line -1))
    (forward-line 1)
    (while (my-align-detect-pattern pattern)
      (goto-char(my-align-detect-pattern pattern))
      (funcall callback)
      (forward-line 1))))

(defmacro my-align-with-neighbours (pattern &rest body)
  "Walk on all neighbour lines with PATTERN and apply BODY to them.

Current point when body is executed is set to match point.

See `my-align-apply-to-neighbours' for more information."
  (declare (indent defun))
  `(my-align-apply-to-neighbours ,pattern
                                 (lambda ()
                                   ,@body)))

(defun my-align-pattern (pattern)
  "Align all neighbour lines so that occurences of PATTERN are on the same column."
  (interactive "sPattern: ")
  (let ((max-col 0))
    (save-excursion
      (my-align-with-neighbours pattern
        (setq max-col (max max-col (current-column))))
      (when (> max-col 0)
        (my-align-with-neighbours pattern
          (let ((offset (- max-col (current-column))))
            (dotimes (_ offset)
              (insert " "))))))))

;; Enabled features
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(provide 'init/ergo)

;;; ergo.el ends here
