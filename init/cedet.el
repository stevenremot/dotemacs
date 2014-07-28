;;; cedet.el --- CEDET initialization module

;;; Commentary:
;;
;;; Code:
(defvar my-site-lisp)
(load-file (concat my-site-lisp "cedet/cedet-devel-load.el"))
(load-file (concat my-site-lisp "cedet/contrib/cedet-contrib-load.el"))

(global-ede-mode 1)

(semantic-load-enable-gaudy-code-helpers)
(semantic-load-enable-all-exuberent-ctags-support)

(global-set-key (kbd "C-c -") 'senator-fold-tag-toggle)

(provide 'init/cedet)

;;; cedet.el ends here
