;;; web.el --- Web mode init module

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(use-package web-mode
  :mode "\\.p?html\\'")

(defmacro my-define-srecode-inserter (name template-name)
  "Create a template caller function.

NAME is the function's name
TEMPLATE-NAME is the name of the template."
  `(defun ,name ()
     ,(concat "Insert template " template-name)
     (interactive)
     (srecode-insert ,template-name)))

(my-define-srecode-inserter my-php-insert-classdef "declaration:php-classdef")
(my-define-srecode-inserter my-php-insert-attrdef "declaration:php-attrdef")
(my-define-srecode-inserter my-php-insert-methoddef "declaration:php-methoddef")


(use-package php-mode
  :bind (("C-c s c" . my-php-insert-classdef)
         ("C-c s a" . my-php-insert-attrdef)
         ("C-c s m" . my-php-insert-methoddef)))

(use-package tern
  :init (add-hook 'js-mode-hook (lambda () (tern-mode t))))

(use-package company-tern
  :init (add-hook 'js-mode-hook (lambda () (company-tern t))))

(defvar my-site-lisp)
(add-to-list 'load-path (concat my-site-lisp "gulpjs/"))
(autoload 'gulpjs-start-task "gulpjs" "Start a gulp task." t)

(provide 'init/web)

;;; web.el ends here
