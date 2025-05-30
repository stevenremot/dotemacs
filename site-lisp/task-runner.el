;;; task-runner.el --- Run various tasks in my projects

;;; Commentary:
;;
(require 'vterm)

;;; Code:
(require 'json)

(defun task-runner-find-tasks/npm ()
  "Find package.json and return scripts as an alist."
  (let* ((dir (if buffer-file-name
                  (file-name-directory buffer-file-name)
                default-directory))
         (package-json-dir (locate-dominating-file dir "package.json")))
    (if package-json-dir
        (let* ((package-json-file (expand-file-name "package.json" package-json-dir))
               (json-object-type 'alist)
               (json-array-type 'list)
               (json-key-type 'string)
               (package-data (json-read-file package-json-file)))
          (mapcar #'(lambda (pair) (cons (car pair) (format "npm run %s" (car pair))))
		  (cdr (assoc "scripts" package-data))))
      (message "No package.json found in the current or parent directories.")
      nil)))

(defun task-runner-start-task/string (name command)
  "Run the task COMMAND.
It is run in a vterm, with the name NAME.
If the task already exists, just switch to the terminal."
  (let* ((dir-name (project-root (project-current)))
	 (buffer-name (format "*task-%s (%s)*" name dir-name))
         (buffer (get-buffer buffer-name)))
    (if buffer
        (switch-to-buffer buffer)
      (let ((default-directory (or (locate-dominating-file default-directory "package.json")
                                   default-directory)))
        (vterm buffer-name)
        (vterm-send-string command)
        (vterm-send-return)))
    (get-buffer buffer-name)))


(defun task-runner-start-task (name command)
  "Run the task named NAME with the command COMMAND.

 COMMAND is a function, execute it in the root directory.
If COMMAND is a string, execute it in a vterm."
  (if (stringp command)
      (task-runner-start-task/string name command)
    (funcall command)))

(require 'vterm)

(defun task-runner-run-task ()
  "Interactively select and run a tasks."
  (interactive)
  (let ((tasks (task-runner-find-tasks/npm)))
    (if tasks
        (let* ((task-names (mapcar #'car tasks))
               (selected-task (completing-read "Select task: " task-names nil t))
               (selected-command (cdr (assoc selected-task tasks))))
          (if selected-command
              (task-runner-start-task selected-task selected-command)
            (message "No command found for the selected task.")))
      (message "No tasks found."))))


(provide 'task-runner)

;;; task-runner.el ends here
