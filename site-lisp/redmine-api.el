;;; redmine-api.el --- Thin wrapper around the redmine Rest API -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:
(require 'cl-lib)
(require 'request)

(defcustom redmine-api-project-url ""
  "Redmine forge URL."
  :type 'string)

(cl-defun redmine-api--request (path &key type success error)
  "Low-level function to make a network call.

PATH is the URL path
TYPE is the HTTP verb
SUCCESS is the success callback
ERROR is the error callback"
  (let* ((host (url-host (url-generic-parse-url redmine-api-project-url)))
	 (found-secret (nth 0 (auth-source-search :max 1 :host host)))
	 (secret-entry (plist-get found-secret :secret))
	 (token (if (functionp secret-entry)
		    (funcall secret-entry)
		  secret-entry)))
    (request (format "%s%s" redmine-api-project-url path)
	     :type type
	     :headers `(("X-Redmine-API-Key" . ,token)
			("Content-Type" . "application/json"))
	     :parser 'json-read
	     :success success
	     :error error)))

(cl-defun redmine-api-get-issue (issue-id &key success error)
  "Return the issue with the given ISSUE-ID.

SUCCESS is the success callback
ERROR is the error callback"
  (redmine-api--request (format "/issues.json?issue_id=%s" issue-id)
			:type "GET"
			:success #'(lambda (&rest data)
				     (let-alist (plist-get data :data)
				       (if (= 0 (length .issues))
					   (funcall success nil)
					 (funcall success (seq-first .issues)))))
			:error error))

(cl-defun redmine-api-list-issues (project-id &key success error)
  "Return a list of issues for PROJECT-ID."

  (redmine-api--request (format "/issues.json?project_id=%s" project-id)
			:type "GET"
			:success #'(lambda (&rest data)
				     (let-alist (plist-get data :data)
				       (funcall success .issues)))
			:error error))

(cl-defun redmine-api-list-versions (project-id &key success error)
  "Return a list of versions for PROJECT-ID."

  (redmine-api--request (format "/projects/%s/versions.json" project-id)
			:type "GET"
			:success #'(lambda (&rest data)
				     (let-alist (plist-get data :data)
				       (funcall success .versions)))
			:error error))

(provide 'redmine-api)

;;; redmine-api.el ends here
