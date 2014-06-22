;;; web.el --- Web mode init module

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(use-package web-mode
  :mode "\\.p?html\\'")

(use-package php-mode)

(use-package tern
  :init (add-hook 'js-mode-hook (lambda () (tern-mode t))))

(use-package company-tern
  :init (add-hook 'js-mode-hook (lambda () (company-tern t))))

(defvar my-site-lisp)
(add-to-list 'load-path (concat my-site-lisp "gulpjs/"))
(autoload 'gulpjs-start-task "gulpjs" "Start a gulp task." t)

(provide 'init/web)

;;; web.el ends here
