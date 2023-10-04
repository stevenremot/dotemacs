;;; test-watcher.el --- Test watcher -*- lexical-binding: t -*-

;;; Commentary:
;;

(require 'project)
(require 'cl-lib)

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
  (require 'vterm)
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

;;;###autoload
(defun tw-junit-org-report-for-buffer ()
  "Create an org report of the currently visited junit.xml file."
  (interactive)
  (let ((junit-data (libxml-parse-xml-region (point-min) (point-max)))
	(report-buffer (get-buffer-create "*junit-org*")))
    (with-current-buffer report-buffer
      (erase-buffer)

      (let-alist (nth 1 junit-data)
	(insert (format "#+TITLE: %s\n\n" .name)))

      (cl-loop for suite-pair in (cddr junit-data)
	       for suite-attrs = (nth 1 suite-pair)
	       for suite-elements = (cddr suite-pair)
	       do
	       (let-alist suite-attrs
		 (insert (format "* %s\n\n" .name))))

      (org-mode)
      (display-buffer (current-buffer)))))

(provide 'test-watcher)

;;; test-watcher.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("tw-" . "test-watcher-"))
;; End:
