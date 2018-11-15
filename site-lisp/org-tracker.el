;;; org-trackers.el --- Opens references to your issues in org mode -*- lexical-binding: t -*-

;;; Commentary:
;; This package allows you to write reference to issues in your org
;; items, and to easily open them in your browser.

;;; Code:
(require 'subr-x)
(require 'org-clock)
(require 'org-agenda)
(require 'elmine)

;; Generic code

(defun org-tracker-get-ref-at-point ()
  "Return the reference currently at point."
  (number-to-string (thing-at-point 'number)))

(defun org-tracker-get-ref-in-string (string)
  "Return first issue reference in STRING."
  (when (numberp (string-match (rx (1+ num)) string))
      (string-to-number (match-string 0 string))))

(defun org-tracker-get-system (pom)
  "Return the ref system to use at POM."
  (org-entry-get pom "tracker" t))

(defun org-tracker-open-issue (pom reference)
  "Open the reference currently at POM.

POM is a point in the org document.
REFERENCE is the reference."
  (let* ((tracker (org-tracker-get-system pom))
	 (func (intern (concat "org-tracker-open-issue:" tracker))))
    (unless (fboundp func)
      (error "Function org-tracker-open-issue:%s does not exist" tracker))
    (funcall func pom reference)))

;;;###autoload
(defun org-tracker-open-issue-at-point ()
  "Open the reference at point."
  (interactive)
  (save-window-excursion
    (let ((reference (org-tracker-get-ref-at-point))
	  (marker (org-agenda-get-any-marker)))
      (when (eq major-mode 'org-agenda-mode)
	(switch-to-buffer (marker-buffer marker))
	(goto-char (marker-position marker)))
      (org-tracker-open-issue (point) reference))))

(defun org-tracker--get-current-issue ()
  "Return the reference in the org item currently clocked in."
  (org-tracker-get-ref-in-string org-clock-current-task))

;;;###autoload
(defun org-tracker-open-current-issue ()
  "Open the reference of the org issue currently clocked in."
  (interactive)
  (let ((reference (org-tracker--get-current-issue)))
    (unless reference
      (error "No item currently clocked in"))
    (org-tracker-open-issue org-clock-marker reference)))

;; Time tracking

(defun org-tracker--wrap-in-list (params)
  "Wrap string PARAMS in a list."
  (concat "(" params ")"))

(defun org-tracker--get-arguments-from-clocktable-def (element)
  "Return the clocktable arguments from its parsed definition.

ELEMENT is the parsed definition, got from the org element API."
  (thread-first
      element
    cadr
    (plist-get :arguments)
    org-tracker--wrap-in-list
    read
    (append '(:properties ("tracker" "redmine-repo" "gitlab-repo")))))

(cl-defstruct org-tracker-command
  properties
  issue-reference
  hours
  result)

(defun org-tracker--pair-to-plist (pair)
  "Transform an alist PAIR into a plist."
  (list (intern (concat ":" (car pair)))
	(cdr pair)))

(defun org-tracker--alist-to-plist (alist)
  "Transform an ALIST into a plist."
  (apply #'append (seq-map #'org-tracker--pair-to-plist alist)))

(defun org-tracker-track-clocktable-time ()
  "Track time for the org clocktable at point."
  (interactive)
					; Retrieve clocktable netrics
  (pcase (org-in-clocktable-p)
    (`nil (error "No clocktable found at point"))
    (start (goto-char start)))

  (let* ((clocktable-args (org-tracker--get-arguments-from-clocktable-def (org-element-at-point)))
	 (clocktable (org-clock-get-table-data "file"
					       (append clocktable-args
						       '(:properties ("tracker" "redmine-repo" "gitlab-repo")
								     :inherit-props t))))

					; Gather commands to perform and properties
	 (commands (cl-loop for (_level title _whatever minutes properties) in (nth 2 clocktable)
			    for issue = (org-tracker-get-ref-in-string title)
			    for plist-properties = (org-tracker--alist-to-plist properties)
			    when (not (null issue))
			    collect (make-org-tracker-command
				     :properties plist-properties
				     :issue-reference issue
				     :hours (/ minutes 60.0)))))
    (cl-loop for command in commands
	     do (org-tracker-track-time (org-tracker-command-properties command)
					(org-tracker-command-issue-reference command)
					(org-tracker-command-hours command)
					(format-time-string "%Y-%m-%d")
					#'(lambda (result) (setf (org-tracker-command-result command) result))))

    (org-tracker--display-tracking-results commands))
  )

(defun org-tracker-track-time (properties issue-id hours date callback)
  "Track time in an issue.

PROPERTIES is a property list for the configuration.  The `:tracker'
entry is used to know which tracker should be used.
ISSUE-ID is the id of the issue.
HOURS is the number of spent hours.
DATE is the date the time was spent.
CALLBACK is the function called when the time is tracked."
  (let* ((tracker (plist-get properties :tracker))
	 (track-func (intern (concat "org-tracker-track-time:" tracker))))
    (if (fboundp track-func)
	(apply track-func properties issue-id date hours callback ())
      (funcall callback `(error ,(format "No tracker found for %S" tracker))))
    ))

(defun org-tracker--display-tracking-results (commands)
  "Display the tracking results in a new buffer.

COMMANDS is a list of commands that should be displayed with their summary."
  (with-current-buffer (get-buffer-create " *org-tracker*")
    (erase-buffer)
    (cl-loop for command in commands
	     do (insert (concat (org-tracker--format-command command) "\n")))
    (display-buffer (current-buffer))))

(defun org-tracker--format-command (command)
  "Return a string that represents the ummary of a COMMAND."
  (format "> %s @ %s\n\tStatus: %s\n"
	  (plist-get (org-tracker-command-properties command) :tracker)
	  (org-tracker-command-issue-reference command)
	  (if (eq 'error (car (org-tracker-command-result command)))
	      (format "Error - %s" (cdr (org-tracker-command-result command)))
	    "Success")))

;; Redmine

(defun org-tracker-open-issue:redmine (pom reference)
  "Open a reference in redmine.

POM is a point in the org document.
REFERENCE is the reference."
  (let ((url (org-entry-get pom "redmine-repo" t)))
    (unless url
      (error "The property \"redmine-repo\” must be set"))
    (browse-url (format "%s/issues/%s" url reference))))

(defun org-tracker-track-time:redmine (properties issue-id date hours callback)
  "Track time in a redmine issue.

PROPERTIES is a property list for the configuration.
ISSUE-ID is the id of the issue.
DATE is the date the time was spent.
HOURS is the number of spent hours.
CALLBACK is the function called when the time is tracked."
  (let ((elmine/host (plist-get properties :redmine-repo))
	(elmine/api-key (plist-get properties :redmine-api-key)))
    (funcall callback (elmine/create-time-entry :issue_id issue-id :date date :hours hours))))

;; Gitlab

(defun org-tracker-open-issue:gitlab (pom reference)
  "Open a reference in gitlab.

POM is a point in the org document.
REFERENCE is the reference."
  (let ((url (org-entry-get pom "gitlab-repo" t)))
    (unless url
      (error "The property \"gitlab-repo\” must be set"))
    (browse-url (format "%s/issues/%s" url reference))))

(provide 'org-tracker)

;;; org-tracker.el ends here
