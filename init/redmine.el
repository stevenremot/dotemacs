;;; redmine.el --- Redmine tools

;;; Commentary:
;;

;;; Code:
(defgroup my-redmine () "Redmine tools" :group 'tools)

(defcustom my-redmine/repo-url ""
  "Repository URL."
  :type 'string
  :group 'my-redmine)

(defun my-redmine/browse-issue (issue-number)
  "Visit the URL corresponding to the given ISSUE-NUMBER."
  (browse-url (format "%s/issues/%s" my-redmine/repo-url issue-number)))

(defun my-redmine/browse-issue-at-point (issue-number)
  "Visit the URL corresponding to the ISSUE-NUMBER at point."
  (interactive (list (number-to-string (thing-at-point 'number))))
  (my-redmine/browse-issue issue-number))

(global-set-key (kbd "C-c r j") #'my-redmine/browse-issue-at-point)

;;; redmine.el ends here
