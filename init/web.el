;;; web.el --- Web mode init module

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(use-package web-mode
  :ensure web-mode
  :mode "\\.p?html\\'"
  :config (progn
            (setq web-mode-engines-alist
                  '(("php" . "\\.phtml\\'")))
            (add-hook 'web-mode-hook (lambda ()
                                            (emmet-mode 1)
                                            (skewer-mode 1)))))



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
  (insert (format " * @param %s %s\n"
                  (if (and (stringp type)
                           (not (string= type "")))
                      type
                    "mixed")
                  name)))

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

(defun my-php-write-doc (doc)
  "Insert DOC at the current point."
  (let ((point nil))
    (insert "/**\n")
    ;; Insert lines
    (insert " */")
    (when point
      ;; Go to point
      )))

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
         ("C-c s d" . my-php-generate-func-doc))
  :config (add-hook 'php-mode-hook
                    (lambda ()
                      (defvar company-backends)
                      (defvar company-semantic-modes)
                      ;; We narrow company to only semantic and GNU Global
                      (set (make-local-variable 'company-backends) '(company-semantic company-gtags))
                      (add-to-list 'company-semantic-modes 'php-mode)
                      ;; php-mode removes whitespace hook, let's add it again
                      (add-hook 'before-save-hook 'delete-trailing-whitespace))))

(use-package js2-mode
  :ensure js2-mode
  :config (progn
            (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
            (add-hook 'js2-mode-hook 'skewer-mode)))

(use-package js2-refactor
  :ensure js2-refactor
  :config (js2r-add-keybindings-with-prefix "C-S-r"))

(use-package css-mode
  :config (add-hook 'css-mode-hook 'skewer-mode))

(use-package skewer-mode
  :ensure skewer-mode)

(defun my-web-kill-skewer-snippet ()
  "Put the snippet to make a webpage skewer-aware in the clipboard."
  (interactive)
  (kill-new "(function () {
    var s = document.createElement(\"script\");
    s.src = \"//localhost:8080/skewer\";
    document.getElementsByTagName(\"head\")[0].appendChild(s);
})()"))

;; Tern
(use-package tern
  :ensure tern
  :init (add-hook 'js2-mode-hook (lambda () (tern-mode t))))

(use-package company-tern
  :ensure company-tern
  :init (progn
          (add-to-list 'company-backends 'company-tern)
          (add-hook 'js2-mode-hook (lambda () (company-tern t)))))

;; Emmet
(use-package emmet-mode
  :ensure emmet-mode
  :bind (("C-c C-c RET" . emmet-expand-line)
         ("S-<left>" . emmet-prev-edit-point)
         ("S-<right>" . emmet-next-edit-point)))

(defvar my-site-lisp)
(add-to-list 'load-path (concat my-site-lisp "gulpjs/"))
(autoload 'gulpjs-start-task "gulpjs" "Start a gulp task." t)

;; Geben
(fset 'my-open-file-geben
   [?\M-x ?m ?y ?  ?k ?i ?l ?l ?  ?f ?i ?l ?e ?  ?n ?a ?m ?e return ?\C-x ?o ?\C-c ?f ?\C-a ?\C-k ?\C-v ?\M-y return])

(use-package rainbow-mode
  :init (progn (add-hook 'css-mode-hook 'rainbow-mode)
               (add-hook 'less-css-mode-hook 'rainbow-mode)))

(provide 'init/web)

;;; web.el ends here
