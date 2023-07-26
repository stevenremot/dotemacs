;;; test-watcher.el --- Test watcher -*- lexical-binding: t -*-

;;; Commentary:
;;

(require 'project)
(require 'vterm)

;;; Code:

;;;###autoload
(defcustom tw-shell-commands '("npm test")
  "Shell commands for test watcher MVP."
  :safe 'listp)

;;;###autoload
(defcustom tw-coverage-paths '("coverage/lcov-report/index.html")
  "Directories to the index.html of the coverage report."
  :safe 'listp)

(defvar tw--shell-history nil
  "Shell history.")

;;;###autoload
(defun tw-run ()
  "Run the test command in vterm for the current project."
  (interactive)
  (let ((command (completing-read "Test command: " tw-shell-commands nil nil))
	(default-directory (project-root (project-current))))
    (vterm (format "*%s test*" (thread-first default-directory
					     (directory-file-name)
					     (file-name-base))))
    (vterm-send-string (format "%s\n" command))))

;;;###autoload
(defun tw-coverage ()
  "Show coverage."
  (interactive)
  (let* ((path-to-use (completing-read "Coverage path:" tw-coverage-paths nil nil))
	 (path (expand-file-name path-to-use (project-root (project-current)))))
    (xwidget-webkit-browse-url (format "file://%s" path))))

(provide 'test-watcher)

;;; test-watcher.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("tw-" . "test-watcher-"))
;; End:
