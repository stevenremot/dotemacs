;;; vscode.el --- vscode compat utilities

;;; Commentary:
;;

(require 'json)

;;; Code:

(defun vscode-setup-flymake-eslint ()
  "Setup flymake-eslint according to vscode configuration."
  (when (buffer-file-name)
    (let* ((root (locate-dominating-file (buffer-file-name) ".vscode"))
	 (settings-file (file-name-concat root ".vscode" "settings.json"))
	 (settings (when (file-exists-p settings-file) (json-read-file settings-file))))
    (seq-do
     #'(lambda (work-dir)
	 (let ((dir (if (listp work-dir)
			work-dir
		      (list (cons 'directory work-dir) (cons 'changeProcessCWD t)))))
	   (let ((path (expand-file-name (alist-get 'directory dir) root))
	       (change-process-cwd? (alist-get 'changeProcessCWD dir)))
	   (when (and change-process-cwd?
		      (string-prefix-p path (buffer-file-name)))
	     (setq-local flymake-eslint-project-root path))))
	 )
     (alist-get 'eslint.workingDirectories settings))))
  )

(provide 'vscode)

;;; vscode.el ends here
