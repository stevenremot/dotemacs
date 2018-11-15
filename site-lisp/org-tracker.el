;;; org-trackers.el --- Opens references to your issues in org mode -*- lexical-binding: t -*-

;;; Commentary:
;; This package allows you to write reference to issues in your org
;; items, and to easily open them in your browser.

;;; Code:
(require 'org-clock)
(require 'org-agenda)

;; Generic code

(defun org-tracker-get-ref-at-point ()
  "Return the reference currently at point."
  (number-to-string (thing-at-point 'number)))

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
  (when (and org-clock-current-task
	   (numberp (string-match (rx (1+ num)) org-clock-current-task)))
      (string-to-number (match-string 0 org-clock-current-task))))

;;;###autoload
(defun org-tracker-open-current-issue ()
  "Open the reference of the org issue currently clocked in."
  (interactive)
  (let ((reference (org-tracker--get-current-issue)))
    (unless reference
      (error "No item currently clocked in"))
    (org-tracker-open-issue org-clock-marker reference)))

;; Redmine

(defun org-tracker-open-issue:redmine (pom reference)
  "Open a reference in redmine.

POM is a point in the org document.
REFERENCE is the reference."
  (let ((url (org-entry-get pom "redmine-repo" t)))
    (unless url
      (error "The property \"redmine-repo\” must be set"))
    (browse-url (format "%s/issues/%s" url reference))))

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
