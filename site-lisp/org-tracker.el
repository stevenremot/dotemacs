;;; org-trackers.el --- Opens references to your issues in org mode -*- lexical-binding: t -*-
;; Package-Requires: ((dash "2.14.0") (request "0.3.0"))

;;; Commentary:
;; This package allows you to write reference to issues in your org
;; items, and to easily open them in your browser.

;;; Code:
(require 'eieio)
(require 'subr-x)
(require 'org-clock)
(require 'org-agenda)
(require 'request)
(require 'dash)

;; Generic code
(defvar org-tracker-classes '((dummy . org-tracker-dummy)
			      (redmine . org-tracker-redmine)
			      (gitlab . org-tracker-gitlab))
  "Mapping between tracker names and their classes.")

(defun org-tracker-get-current-tracker (pom)
  "Return an instance of the tracker at POM.

Return nil when no tracker could be created."
  (let* ((tracker (org-entry-get pom "tracker" t))
	 (tracker-class (cdr (assoc (intern tracker) org-tracker-classes))))
    (if tracker-class
	(make-instance tracker-class pom)
      nil)))

(defun org-tracker-get-ref-at-point ()
  "Return the reference currently at point."
  (number-to-string (thing-at-point 'number)))

(defun org-tracker-get-ref-in-string (string)
  "Return first issue reference in STRING."
  (when (numberp (string-match (rx "#" (group (1+ alnum)) word-boundary) string))
    (match-string 1 string)))

(defun org-tracker-get-issue-for-point ()
  "Return the issue reference in POM context.

Go up the item hierarchy until some item contains an issue or
root is reached."
      (->> (org-get-outline-path t)
	 (seq-map #'org-tracker-get-ref-in-string)
	 (seq-find #'stringp)))

(cl-defgeneric org-tracker-open-issue (_tracker _reference)
  "Open the issue at point.

TRACKER is the instance of the tracker.
REFERENCE is the issue reference."
  (error "Method org-tracker-open-issue not implemented for this tracker"))

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
      (org-tracker-open-issue (org-tracker-get-current-tracker (point)) reference))))

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
    (org-tracker-open-issue (org-tracker-get-current-tracker org-clock-marker) reference)))

;; Time tracking

(cl-defmethod org-tracker-track-time (_tracker _issue-id _date _hours _comment _callback)
  "Track time for an issue.

TRACKER is the current tracker instance.
ISSUE-ID is the reference of the issue to track in.
DATE is the date of the day to spend time in emacs timestamp format.
HOURS is the number of hours to spend.
COMMENT is an optional comment string associated to the log.
CALLABCK is a function called when the tracking is completed."
  (error "Method org-tracker-track-time not implemented for this tracker"))

(defun org-tracker-get-timestamp-start-date (timestamp)
  "Return the start date of an org TIMESTAMP element."
  (encode-time 0
	       (org-element-property :minute-start timestamp)
	       (org-element-property :hour-start timestamp)
	       (org-element-property :day-start timestamp)
	       (org-element-property :month-start timestamp)
	       (org-element-property :year-start timestamp)))

(defun org-tracker-get-timestamp-end-date (timestamp)
  "Return the end date of an org TIMESTAMP element."
  (encode-time 0
	       (org-element-property :minute-end timestamp)
	       (org-element-property :hour-end timestamp)
	       (org-element-property :day-end timestamp)
	       (org-element-property :month-end timestamp)
	       (org-element-property :year-end timestamp)))

(defun org-tracker-get-tracking-comment-for-point ()
  "Return the comment that should be associated to the current tracking."
  (let ((outline-path (org-get-outline-path t))
	(comment-list '())
	(passed-issue-id nil))
    (dolist (item outline-path)
      (if passed-issue-id
	  (setq comment-list (push item comment-list))
	(when (stringp (org-tracker-get-ref-in-string item))
	  (setq passed-issue-id t))))
    (mapconcat #'identity (reverse comment-list) " - ")))

(defun org-tracker-track-time-at-point (pom)
  "Track time using the clock entry at POM."
  (interactive "d")
  (let ((element (org-element-at-point))
	(tracker (org-tracker-get-current-tracker pom))
	timestamp start-date end-date duration)
    (unless (eq (org-element-type element) 'clock)
      (error "Point is not on a clock entry"))

    (setq timestamp (org-element-property :value element)
	  start-date (org-tracker-get-timestamp-start-date timestamp)
	  end-date (org-tracker-get-timestamp-end-date timestamp)
	  duration (/ (float-time (time-subtract end-date start-date)) 3600))

    (org-tracker-track-time tracker
			    (org-tracker-get-issue-for-point)
			    start-date
			    duration
			    (org-tracker-get-tracking-comment-for-point)
			    (lambda (&optional err)
			      (if err
				  (error err)
				(message "Timestamp tracked"))))))

;; Dummy tracker - for testing purposes

(defclass org-tracker-dummy ()
  ()
  "Dummy tracker for testing purposes.")

(cl-defmethod initialize-instance ((tracker org-tracker-dummy) _pom)
  (cl-call-next-method tracker))

(cl-defmethod org-tracker-open-issue ((_tracker org-tracker-dummy) reference)
  (message "Browsing issue %s" reference))

(cl-defmethod org-tracker-track-time ((_tracker org-tracker-dummy) issue-id date hours comment callback)
  (message "Tracking %f hours at %s for %s with comment %s"
	   hours
	   (format-time-string "%F" date)
	   issue-id
	   comment)
  (funcall callback))

;; Redmine

(defclass org-tracker-redmine ()
  ((repo :type string :initarg :repo))
  "Tracker for redmine.")

(cl-defmethod initialize-instance ((tracker org-tracker-redmine) pom)
  (cl-call-next-method tracker
		       (list :repo (org-entry-get (car pom) "redmine-repo" t))))

(cl-defmethod org-tracker-open-issue ((tracker org-tracker-redmine) reference)
  (let ((url (oref tracker repo)))
    (browse-url (format "%s/issues/%s" url reference))))

(cl-defmethod org-tracker-track-time ((tracker org-tracker-redmine) issue-id date hours comment callback)
  "Track time in a redmine issue.

PROPERTIES is a property list for the configuration.
ISSUE-ID is the id of the issue.
DATE is the date the time was spent.
HOURS is the number of spent hours.
CALLBACK is the function called when the time is tracked."
  (let* ((host (url-host (url-generic-parse-url (oref tracker repo))))
	 (found-secret (nth 0 (auth-source-search :max 1 :host host)))
	 (secret-entry (plist-get found-secret :secret))
	 (token (if (functionp secret-entry)
		    (funcall secret-entry)
		  secret-entry)))

    (request (format "%s/time_entries.json" (oref tracker repo))
	     :type "POST"
	     :headers `(("X-Redmine-API-Key" . ,token)
			("Content-Type" . "application/json"))
	     :parser 'json-read
	     :data (json-encode
		    `(("time_entry" . (("issue_id" . ,issue-id)
				       ("spent_on" . ,(format-time-string "%F" date))
				       ("hours" . ,hours)
				       ("comments" . ,comment)))))
	     :success (cl-function (lambda (&rest)
				     (funcall callback nil)))
	     :error (cl-function (lambda (&rest args)
				   (message "%S" args)
				   (funcall callback "Error"))))))

;; Gitlab
(defclass org-tracker-gitlab ()
  ((repo :type string :initarg :repo)
   (project :type string :initarg :project))
  "Tracker for gitlab.")

(cl-defmethod initialize-instance ((tracker org-tracker-gitlab) pom)
  (cl-call-next-method tracker
		       (list :repo (org-entry-get (car pom) "gitlab-repo" t)
			     :project (org-entry-get (car pom) "gitlab-project" t))))

(cl-defmethod org-tracker-open-issue ((tracker org-tracker-gitlab) reference)
  (let ((url (oref tracker repo))
	(project (oref tracker project)))
    (browse-url (format "%s/%s/issues/%s" url project reference))))

(cl-defmethod org-tracker-track-time ((tracker org-tracker-gitlab) issue-id _date hours comment callback)
  (let* ((host (url-host (url-generic-parse-url (oref tracker repo))))
	 (found-secret (nth 0 (auth-source-search :max 1 :host host)))
	 (secret-entry (plist-get found-secret :secret))
	 (token (if (functionp secret-entry)
		    (funcall secret-entry)
		  secret-entry)))

    (unless token
      (error "Cannot find a auth entry for host %s" host))

    (when (> (length comment) 0)
      (request
       (format "%s/api/v4/projects/%s/issues/%s/notes"
	       (oref tracker repo)
	       (url-hexify-string (oref tracker project))
	       issue-id)
       :type "POST"
       :headers `(("PRIVATE-TOKEN" . ,token))
       :params `(("body" . ,comment))))

    (request
     (format "%s/api/v4/projects/%s/issues/%s/add_spent_time"
	     (oref tracker repo)
	     (url-hexify-string (oref tracker project))
	     issue-id)
     :type "POST"
     :headers `(("PRIVATE-TOKEN" . ,token))
     :params `(("duration" . ,(format "%fh" hours)))
     :success (cl-function
	       (lambda (&rest arg)
		 (funcall callback nil))))))

(provide 'org-tracker)

;;; org-tracker.el ends here
