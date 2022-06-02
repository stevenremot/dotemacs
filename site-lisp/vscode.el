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
	 (settings (json-read-file settings-file)))
    (seq-do
     #'(lambda (work-dir)
	 (let ((path (expand-file-name (alist-get 'directory work-dir) root))
	       (change-process-cwd? (alist-get 'changeProcessCWD work-dir)))
	   (when (and change-process-cwd?
		      (string-prefix-p path (buffer-file-name)))
	     (setq-local flymake-eslint-project-root path))))
     (alist-get 'eslint.workingDirectories settings))))
  )

(provide 'vscode)

;;; vscode.el ends here
