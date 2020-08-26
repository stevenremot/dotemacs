;;; redmine-helpers.el --- Helpers for redmine management -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:
(require 'org-capture)
(require 'request)

(defcustom my-redmine-project-url ""
  "Redmine forge URL."
  :type 'string)

;;;###autoload
(defun my-redmine-capture-issue (issue-id template)
  "Start a capturing a redmine issue with id ISSUE-ID and set it using capture TEMPLATE."
  (interactive (list (read-string "Issue: ")
		     (car (org-capture-select-template))))
  (let* ((host (url-host (url-generic-parse-url my-redmine-project-url)))
	 (found-secret (nth 0 (auth-source-search :max 1 :host host)))
	 (secret-entry (plist-get found-secret :secret))
	 (token (if (functionp secret-entry)
		    (funcall secret-entry)
		  secret-entry)))

    (request (format "%s/issues.json?issue_id=%s" my-redmine-project-url issue-id)
	     :type "GET"
	     :headers `(("X-Redmine-API-Key" . ,token)
			("Content-Type" . "application/json"))
	     :parser 'json-read
	     :success #'(lambda (&rest data)
			  (let-alist (plist-get data :data)
			    (let-alist (seq-first .issues)
			      (org-capture nil template)
			      (insert (format "%s - %s" issue-id .subject)))))
	     :error #'(lambda (&rest args)
			(error "%S" args)))))

(provide 'redmine-helpers)

;;; redmine-helpers.el ends here
