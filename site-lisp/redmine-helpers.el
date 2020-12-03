;;; redmine-helpers.el --- Helpers for redmine management -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:
(require 'org-capture)
(require 'redmine-api)

;;;###autoload
(defun my-redmine-capture-issue (issue-id template)
  "Start a capturing a redmine issue with id ISSUE-ID and set it using capture TEMPLATE."
  (interactive (list (read-string "Issue: ")
		     (car (org-capture-select-template))))
  (redmine-api-get-issue
   issue-id
   :success #'(lambda (issue)
		(let-alist issue
		  (org-capture nil template)
		  (insert (format "%s - %s" issue-id .subject))))
   :error #'(lambda (&rest args)
	      (error "%S" args))))

(provide 'redmine-helpers)

;;; redmine-helpers.el ends here
