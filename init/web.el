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

(defun my-php-write-param-doc-line (type name)
  "Insert a PHPDoc line for a param.

TYPE is the type of the parameter.
NAME is the name of the parameter."
  (insert (format " * @param %s %s\n" type name)))

(defun my-php-generate-func-doc ()
  "Generate documentation for a function tag."
  (interactive)
  (let* ((tag (semantic-current-tag))
         (args (semantic-tag-function-arguments tag))
         base-point)
    (php-beginning-of-defun)
    (open-line 1)
    (setq base-point (point))
    (insert "/**\n")
    (dolist (arg args)
      (my-php-write-param-doc-line "multitype:" (semantic-tag-name arg)))
    (insert " *\n")
    (insert (format " * @return %s\n" (read-string "Return type : ")))
    (insert " */")
    (indent-region base-point (point))))

(use-package php-mode
  :bind (("C-c s c" . my-php-insert-classdef)
         ("C-c s a" . my-php-insert-attrdef)
         ("C-c s m" . my-php-insert-methoddef)
         ("C-c s d" . my-php-generate-func-doc)))

(use-package tern
  :init (add-hook 'js-mode-hook (lambda () (tern-mode t))))

(use-package company-tern
  :init (add-hook 'js-mode-hook (lambda () (company-tern t))))

(defvar my-site-lisp)
(add-to-list 'load-path (concat my-site-lisp "gulpjs/"))
(autoload 'gulpjs-start-task "gulpjs" "Start a gulp task." t)

(provide 'init/web)

;;; web.el ends here
