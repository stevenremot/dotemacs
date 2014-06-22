;;; custom.el --- Configure custom conf

;;; Commentary:
;;
;;; Code:

(defvar my-confdir)
(defconst my-custom-conf-file (concat my-confdir "custom-configuration.el"))

(setq custom-file my-custom-conf-file)
(load-file my-custom-conf-file)

(provide 'custom)

;;; custom.el ends here
