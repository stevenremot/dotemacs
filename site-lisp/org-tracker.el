;;; org-trackers.el --- Opens references to your issues in org mode -*- lexical-binding: t -*-

;;; Commentary:
;; This package allows you to write reference to issues in your org
;; items, and to easily open them in your browser.

;;; Code:
(require 'eieio)
(require 'subr-x)
(require 'org-clock)
(require 'org-agenda)
(require 'elmine)

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
  (when (numberp (string-match (rx (1+ num)) string))
      (string-to-number (match-string 0 string))))

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
    (org-tracker-open-issue org-clock-marker reference)))

;; Time tracking

(cl-defmethod org-tracker-track-time (_tracker _issue-id _date _hours _callback)
  "Track time for an issue.

TRACKER is the current tracker instance.
ISSUE-ID is the reference of the issue to track in.
DATE is the date of the day to spend time in (YYYY-MM-DD).
HOURS is the number of hours to spend.
CALLABCK is a function called when the tracking is completed."
  (error "Method org-tracker-track-time not implemented for this tracker"))

;; Dummy tracker - for testing purposes

(defclass org-tracker-dummy ()
  ()
  "Dummy tracker for testing purposes.")

(cl-defmethod initialize-instance ((tracker org-tracker-dummy) _pom)
  (cl-call-next-method tracker))

(cl-defmethod org-tracker-open-issue ((_tracker org-tracker-dummy) reference)
  (message "Browsing issue %s" reference))

(cl-defmethod org-tracker-track-time ((_tracker org-tracker-dummy) issue-id date hours callback)
  (message "Tracking %f hours at %s for %s" hours date issue-id)
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

;; (defun org-tracker-track-time ((tracker org-tracker-redmine) issue-id date hours callback)
;;   "Track time in a redmine issue.

;; PROPERTIES is a property list for the configuration.
;; ISSUE-ID is the id of the issue.
;; DATE is the date the time was spent.
;; HOURS is the number of spent hours.
;; CALLBACK is the function called when the time is tracked."
;;   (let ((elmine/host (plist-get properties :redmine-repo))
;; 	(elmine/api-key (plist-get properties :redmine-api-key)))
;;     (funcall callback (elmine/create-time-entry :issue_id issue-id :date date :hours hours))))

;; Gitlab
(defclass org-tracker-gitlab ()
  ((repo :type string :initarg :repo))
  "Tracker for gitlab.")

(cl-defmethod initialize-instance ((tracker org-tracker-gitlab) pom)
  (cl-call-next-method tracker
		       (list :repo (org-entry-get (car pom) "gitlab-repo" t))))

(cl-defmethod org-tracker-open-issue ((tracker org-tracker-gitlab) reference)
  (let ((url (oref tracker repo)))
    (browse-url (format "%s/issues/%s" url reference))))

(provide 'org-tracker)

;;; org-tracker.el ends here
