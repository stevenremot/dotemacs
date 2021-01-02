;;; remote.el --- HTTP interface for a basic logger
;; -*- lexical-binding: t -*-

;;; Commentary:
;;

(require 'simple-httpd)

;;; Code:
(defconst remote-debug-dir (file-name-directory (or load-file-name buffer-file-name)))

(defun remote-debug-launch ()
  (interactive)

  (defservlet remote-debug/script.js application/javascript ()
    (insert-file-contents-literally
     (expand-file-name "remote-debug.js"
		       remote-debug-dir)))

  (defservlet remote-debug/log application.json ()
    (let-alist (json-read-from-string
		(cadr (assoc "Content" request)))
      (with-current-buffer (get-buffer-create "*remote-log*")
	(insert (concat .level ": " .message "\n"))))))

(provide 'remote-debug)

;;; remote.el ends here
