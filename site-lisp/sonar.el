;;; sonar.el --- Sonar helpers

;;; Commentary:
;;

;;; Code:

(defcustom sonar-url ""
  "Sonar base URL.")

;;;###autoload
(put 'sonar-url 'safe-local-variable 'stringp)

(defcustom sonar-project-id ""
  "Sonar project ID.")

;;;###autoload
(put 'sonar-project-id 'safe-local-variable 'stringp)

;;;###autoload
(defun sonar-visit-file-page ()
  "Open the sonar page with the file issues in the web browser."
  (interactive)
  (let ((file-name (buffer-file-name (current-buffer)))
	(project-directory (vc-root-dir)))
    (browse-url (format "%s/code?id=%s&selected=%s:%s" sonar-url sonar-project-id sonar-project-id (file-relative-name file-name project-directory)))))

(provide 'sonar)

;;; sonar.el ends here
