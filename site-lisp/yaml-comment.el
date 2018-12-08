;;; yaml-comment.el --- Edit a comment in yml format -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:

(defvar-local yaml-comment--origin-buffer nil
  "Original buffer of the current comment.")

(defvar-local yaml-comment--origin-beg nil
  "Original beginning point of the current comment.")

(defvar-local yaml-comment--origin-end nil
  "Original ending point of the current comment.")

(defmacro yaml-comment--with-comment-config (&rest body)
  "Execute BODY with comment configuration set."
  (declare (indent defun))
  `(let ((comment-start "/**")
	(comment-start-skip (rx (group "/**") (0+ (or whitespace) "\n") " * "))
	(comment-end " */")
	(comment-end-skip (rx (0+ (or whitespace) "\n") (group "*/")))
	(comment-continue " * ")
	(comment-style 'extra-line)
	(comment-multi-line t))
    ,@body))

(defun yaml-comment--wrap (beg end)
  "Wrap a region from BEG to END in a comment."
  (yaml-comment--with-comment-config
    (comment-region beg end)))

(defun yaml-comment--unwrap (beg end)
  "Unwrap a region from BEG to END in a comment."
  (yaml-comment--with-comment-config
    (uncomment-region beg end)))

(defun yaml-comment-edit-at-point (point)
  "Start editing the comment at POINT as YAML."
  (interactive "d")
  (save-excursion
    (goto-char point)
    (let* ((yaml-comment-beg (re-search-backward (rx "/**")))
	   (yaml-comment-end (re-search-forward (rx "*/")))
	   (comment-content (buffer-substring yaml-comment-beg yaml-comment-end))
	   (origin-buffer (current-buffer)))
      (with-current-buffer (get-buffer-create "*yaml-edit*")
	(erase-buffer)
	(insert comment-content)
	(yaml-comment--unwrap (point-min) (point-max))
	(yaml-mode)
	(yaml-comment-edit-mode t)

	(setq-local yaml-comment--origin-beg yaml-comment-beg)
	(setq-local yaml-comment--origin-end yaml-comment-end)
	(setq-local yaml-comment--origin-buffer origin-buffer))))
  (switch-to-buffer-other-window "*yaml-edit*"))

(defun yaml-comment-apply-changes ()
  "Apply the modifications done in a Yaml comment edit buffer."
  (interactive)
  (let ((delete-trailing-lines t)
	new-content yaml-comment-beg yaml-comment-end)
    (yaml-comment--wrap (point-min) (point-max))
    (delete-trailing-whitespace)

    (setq new-content (buffer-substring (point-min) (1- (point-max)))
	  yaml-comment-beg yaml-comment--origin-beg
	  yaml-comment-end yaml-comment--origin-end)

    (with-current-buffer yaml-comment--origin-buffer
      (delete-region yaml-comment-beg yaml-comment-end)
      (goto-char yaml-comment-beg)
      (insert new-content))

    (switch-to-buffer-other-window yaml-comment--origin-buffer)
    (kill-buffer "*yaml-edit*")))

(define-minor-mode yaml-comment-edit-mode
  "Toggle yaml comment edition mode."
  nil
  " YAML-Edit"
  `((,(kbd "C-c C-c") . yaml-comment-apply-changes)
    (,(kbd "C-c C-k") . kill-current-buffer))
  :group 'yaml-comment)

(provide 'yaml-comment)

;;; yaml-comment.el ends here
