;;; init.el --- Where all the magic begins
;;

;;; Commentary:
;;
;;; Code:

(defconst my-confdir (file-name-directory (or buffer-file-name load-file-name)))
(defconst my-init-dir (concat my-confdir "init/"))
(defconst my-init-modules '("custom"
                            "package"
                            "theme"))

(defun my-init-modules (modules)
  "Launch MODULES' initialization."
  (dolist (module modules)
    (load-file (concat my-init-dir module ".el"))))

(my-init-modules my-init-modules)

(provide 'init)
;;; init.el ends here
