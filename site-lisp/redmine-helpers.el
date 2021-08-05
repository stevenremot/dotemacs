;;; redmine-helpers.el --- Helpers for redmine management -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:
(require 'org-capture)
(require 'redmine-api)

;;; Dedicated GUI
(defun my-redmine--display-issue-in-buffer (issue)
  "Display fetched ISSUE in a new buffer."
  (let-alist issue
    (with-current-buffer (get-buffer-create (format "*redmine: issue #%s*" .id))
      (erase-buffer)
      (markdown-mode)
      (insert (format "# %s\n\n" .subject))

      (insert-button "Add to Tasks"
		     'action #'(lambda (_button) (my-redmine-insert-issue .id "Traqfood"))
		     'mouse-action #'(lambda (_button) (my-redmine-insert-issue .id "Traqfood")))

      (insert " - ")

      (insert-button "Visit"
		     'action #'(lambda (_button) (my-redmine-insert-issue .id "Traqfood"))
		     'mouse-action #'(lambda (_button) (my-redmine-insert-issue .id "Traqfood")))

      (insert "\n\n")

      (when .assigned_to.name
	(insert (format "**Assigned to:** %s\n\n" .assigned_to.name)))

      (insert (format "**Description:**\n%s\n\n" .description))
      (insert (format "\n\n\n\n\n**Issue content:**\n```lisp\n%S\n```\n" issue))

      (replace-string "" "" nil (point-min) (point-max))
      (switch-to-buffer-other-window (current-buffer)))))

;;;###autoload
(defun my-redmine-show-issue (issue-id)
  "Show the issue ISSUE-ID in a new buffer."
  (interactive (list (read-string "Issue: ")))
  (redmine-api-get-issue issue-id
			 :success #'(lambda (issue) (my-redmine--display-issue-in-buffer issue))
			 :error 'error))

;;; Org mode interaction

(defconst my-redmine-project-regexp (rx line-start "*" (+ whitespace) (group (* (any alnum space))) line-end)
  "Regexp for matching a project section.")

(defun my-redmine-list-projects ()
  "List all the available projects in the buffer."
  (save-excursion
    (goto-char (point-min))

    (let ((projects '()))
      (while (re-search-forward my-redmine-project-regexp nil :no-error)
	(setq projects (push (match-string 1) projects)))
      projects)))

(defun my-redmine-list-task-projects ()
  "List all available projects in tasks buffer."
  (save-excursion
    (bookmark-jump "Tasks")
    (my-redmine-list-projects)))

(defun my-redmine-goto-task-project (project)
  "Go to a specific PROJECT."
  (bookmark-jump "Tasks")
  (goto-char (point-min))
  (re-search-forward (rx-to-string `(group line-start "*" (+ whitespace) ,project)))

  (when (save-excursion
	  (next-line)
	  (org-at-property-block-p))
    (next-line)
    (goto-char (cdr (org-get-property-block))))

  (end-of-line))


;;;###autoload
(defun my-redmine-insert-issue (issue-id project)
  "Insert a redmine issue with id ISSUE-ID as a level 2 org title under PROJECT."
  (interactive (list (read-string "Issue: ")
		     (completing-read "Project: " (my-redmine-list-task-projects))))
  (redmine-api-get-issue
   issue-id
   :success #'(lambda (issue)
		(let-alist issue
		  (my-redmine-goto-task-project project)
		  (org-insert-todo-subheading (point))
		  (insert (format "#%s - %s" issue-id .subject))))
   :error #'(lambda (&rest args)
	      (error "%S" args))))

(provide 'redmine-helpers)

;;; redmine-helpers.el ends here
