;;; web.el --- Web mode init module

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(use-package web-mode
  :ensure web-mode
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
  (insert (format " * @param %s %s\n" (if type type "mixed") name)))

(defun my-php-get-func-arguments (tag)
  "Return arguments of TAG function.

Return a result as '((type1 arg1) (type2 arg2) ... )"
  (mapcar
   (lambda (arg-tag)
     (list (semantic-tag-type arg-tag)
           (semantic-tag-name arg-tag)))
   (semantic-tag-function-arguments tag)))

(defun my-php-align-col (rows col-number)
  "Align cells of ROWS for column COL-NUMBER.

This operation is done in place."
  (when rows
    (let ((max-length (apply 'max
                             (mapcar
                              (lambda (row) (length (nth (1- col-number) row)))
                              rows))))
      (dolist (row rows)
        (let ((cell (nth (1- col-number) row))
              (spaces ""))
          (dotimes (i (- max-length (length cell)))
            (setq spaces (concat spaces " ")))
          (setf (nth (1- col-number) row) (concat cell spaces)))))))

(defun my-php-generate-func-doc ()
  "Generate documentation for a function tag."
  (interactive)
  (let* ((tag (semantic-current-tag))
         (args (my-php-get-func-arguments tag))
         base-point
         description-point
         end-point)
    (my-php-align-col args 1)
    (php-beginning-of-defun)
    (open-line 1)
    (setq base-point (point))
    (insert "/**\n")
    (insert " * \n")
    (dolist (arg args)
      (my-php-write-param-doc-line (nth 0 arg) (nth 1 arg)))
    (insert " *\n")
    (insert (format " * @return %s\n" (read-string "Return type : ")))
    (insert " */")
    (indent-region base-point (point))))

(use-package php-mode
  :ensure php-mode
  :bind (("C-c s c" . my-php-insert-classdef)
         ("C-c s a" . my-php-insert-attrdef)
         ("C-c s m" . my-php-insert-methoddef)
         ("C-c s d" . my-php-generate-func-doc)))

(use-package tern
  :ensure tern
  :init (add-hook 'js-mode-hook (lambda () (tern-mode t))))

(use-package company-tern
  :ensure company-tern
  :init (add-hook 'js-mode-hook (lambda () (company-tern t))))

(defvar my-site-lisp)
(add-to-list 'load-path (concat my-site-lisp "gulpjs/"))
(autoload 'gulpjs-start-task "gulpjs" "Start a gulp task." t)

(provide 'init/web)

;;; web.el ends here
