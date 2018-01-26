;;; elpa-backup.el --- Backup system for elpa packages

;;; Commentary:
;;

;;; Code:
(require 'subr-x)

(defconst elpa-backup-file (expand-file-name "elpa-backup.tar.gz" user-emacs-directory)
  "File that contains the elpa backup file.")

(defun elpa-backup-do ()
  "Perform a backup of the installed packages."
  (interactive)
  (let ((default-directory (expand-file-name user-emacs-directory)))
    (start-process "elpa-backup" "*elpa-backup-do*"
		 "tar" "-czf" elpa-backup-file
		 "-C" default-directory
		 (thread-first package-user-dir
		   expand-file-name
		   file-relative-name))))

(defun elpa-backup-rollback ()
  "Apply the backup done previously."
  (interactive)
  (let ((default-directory (expand-file-name user-emacs-directory))
	(package-dir (expand-file-name package-user-dir)))

    (when (file-exists-p package-dir)
      (delete-directory package-dir :recursive))

    (start-process "elpa-backup" "*elpa-backup-do*"
		 "tar" "-xzf" elpa-backup-file
		 "-C" default-directory)))

(provide 'elpa-backup)

;;; elpa-backup.el ends here
