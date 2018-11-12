;;; org-open-refs.el --- Opens references to your issues in org mode -*- lexical-binding: t -*-

;;; Commentary:
;; This package allows you to write reference to issues in your org
;; items, and to easily open them in your browser.

;;; Code:
(require 'org-clock)

(defun org-open-ref-get-ref-at-point ()
  "Return the reference currently at point."
  (number-to-string (thing-at-point 'number)))

(defun org-open-ref--redmine-open (pom reference)
  "Open a reference in redmine.

POM is a point in the org document.
REFERENCE is the reference."
  (let ((url (org-entry-get pom "redmine-repo" t)))
    (unless url
      (error "The property \"redmine-repo\” must be set"))
    (browse-url (format "%s/issues/%s" url reference))))

(defun org-open-ref--gitlab-open (pom reference)
  "Open a reference in gitlab.

POM is a point in the org document.
REFERENCE is the reference."
  (let ((url (org-entry-get pom "gitlab-repo" t)))
    (unless url
      (error "The property \"gitlab-repo\” must be set"))
    (browse-url (format "%s/issues/%s" url reference))))

(defun org-open-ref-get-system (pom)
  "Return the ref system to use at POM."
  (org-entry-get pom "ref-system" t))

(defun org-open-ref (pom reference)
  "Open the reference currently at POM.

POM is a point in the org document.
REFERENCE is the reference."
  (pcase (org-open-ref-get-system pom)
    ("redmine" (org-open-ref--redmine-open pom reference))
    ("gitlab" (org-open-ref--gitlab-open pom reference))
    (_ (error "Could not recognize %S ref-system"(org-open-ref-get-system pom)))))

;;;###autoload
(defun org-open-ref-at-point ()
  "Open the reference at point."
  (interactive)
  (message "%S" (org-entry-properties))
  (org-open-ref (point) (org-open-ref-get-ref-at-point)))

(defun org-open-ref--get-current-issue ()
  "Return the reference in the org item currently clocked in."
  (when (and org-clock-current-task
	   (numberp (string-match (rx (1+ num)) org-clock-current-task)))
      (string-to-number (match-string 0 org-clock-current-task))))

;;;###autoload
(defun org-open-ref-for-current-issue ()
  "Open the reference of the org issue currently clocked in."
  (interactive)
  (let ((reference (org-open-ref--get-current-issue)))
    (unless reference
      (error "No item currently clocked in"))
    (org-open-ref org-clock-marker reference)))


(provide 'org-open-refs)

;;; org-open-refs.el ends here
