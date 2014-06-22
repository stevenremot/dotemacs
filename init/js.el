;;; js.el --- Javascript init module

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(use-package tern
  :init (add-hook 'js-mode-hook (lambda () (tern-mode t))))

(use-package company-tern
  :init (add-hook 'js-mode-hook (lambda () (company-tern t))))

(defvar my-site-lisp)
(add-to-list 'load-path (concat my-site-lisp "gulpjs/"))
(autoload 'gulpjs-start-task "gulpjs" "Start a gulp task." t)

(provide 'init/js)

;;; js.el ends here
