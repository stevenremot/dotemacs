;;; custom.el --- Configure custom conf

;;; Commentary:
;;
;;; Code:

(defvar my-confdir)
(defconst my-custom-conf-file (concat my-confdir "custom-configuration.el"))
(defconst my-site-lisp (concat my-confdir "site-lisp/"))

(setq custom-file my-custom-conf-file)
(load-file my-custom-conf-file)

(add-to-list 'load-path my-site-lisp)

(provide 'init/custom)

;;; custom.el ends here
