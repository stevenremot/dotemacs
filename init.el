;;; init.el --- Main entry point of Emacs configuration

;;; Commentary:
;;

(defconst my-init-dir (file-name-directory (or load-file-name (buffer-file-name))))

(defun my-load-init-file (file)
  (load (concat my-init-dir "init/" file ".el")))

(mapcar #'my-load-init-file
        '("core"
          "theme"
          "ivy"
	  "editing"
	  "git"
	  "project"
	  "emacs-lisp"
	  ))

;;; init.el ends here
