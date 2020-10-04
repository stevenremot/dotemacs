;;; eltojs.el --- Simple Emacs Lisp to JS compiler

;;; Commentary:
;;
;;; Code:

(defconst eltojs--arithmetic-operators '(+ - * /))

(defun eltojs-is-arithmetic-operation? (sexp)
  "Return t when SEXP is an arithmetic operation."
  (and (listp sexp) (member (car sexp) eltojs--arithmetic-operators)))

(defun eltojs-is-function-call? (sexp)
  "Return t when SEXP is calling something."
  (and (listp sexp) (symbolp (car sexp))))

(defun eltojs-is-defun? (sexp)
  "Return non-nil if SEXP is a defun."
  (and (listp sexp) (equal 'defun (car sexp))))

(defun eltojs-is-method-name? (symbol)
  "Return non-nil when SYMBOL is a method name (starts with .)."
  (string-prefix-p "." (symbol-name symbol)))

(defun eltojs-compile-group (sexp)
  "Compile SEXP as a group in an expression.

Basically, put parenthesis around when necesary."
  (pcase sexp
    ((pred eltojs-is-arithmetic-operation?)
     (format "(%s)" (eltojs-compile sexp)))
    (_ (eltojs-compile sexp))))

(defun eltojs-compile-operation (symbol &rest args)
  "Compile an arithmetic operation SYMBOL with its operands ARGS."
  (mapconcat #'eltojs-compile-group args (format " %s " symbol)))

(defun eltojs-prefix-lines (prefix string)
  "Prefix all lines in STRING with PREFIX."
  (mapconcat
   (lambda (line) (concat prefix line))
   (split-string string (rx "\n"))
   "\n"))

(defun eltojs-compile-comma-list (sexps)
  "Compile a list of SEXPS as comma-separated expression."
  (mapconcat #'eltojs-compile sexps ", "))

(defun eltojs-compile-defun (name args docstring-or-body &rest body)
  "Compile arguments as a defun.

NAME is the function name

ARGS is the list of arguments.

DOCSTRING-OR-BODY may be a string that will serve a a
docstring, or the first line of the body.

BODY is the rest of the body."
  (if (stringp docstring-or-body)
      (format
       "/**\n%s\n */\nfunction %s(%s) {\n%s\n}"
       (eltojs-prefix-lines " * " docstring-or-body)
       name
       (eltojs-compile-comma-list args)
       (mapconcat #'(lambda (statement) (concat "  " (eltojs-compile statement) ";")) body "\n"))

    (format
     "function %s(%s) {\n%s\n}"
     name
     (eltojs-compile-comma-list args)
     (mapconcat #'(lambda (statement) (concat "  " (eltojs-compile statement) ";"))
		(append (list docstring-or-body) body)
		"\n")))
  )

(defun eltojs-compile-function-call (symbol &rest args)
  "Compile an expression calling SYMBOL on ARGS."
  (pcase symbol
    ((pred eltojs-is-method-name?)
     (format "%s%s(%s)"
	     (eltojs-compile-group (car args))
	     symbol
	     (eltojs-compile-comma-list (cdr args))))
    (_ (format "%s(%s)"
	       symbol
	       (eltojs-compile-comma-list args)))))

(defun eltojs-compile (sexp)
  "Compile SEXP in javascript."
  (pcase sexp
    ((pred eltojs-is-arithmetic-operation?)
     (apply #'eltojs-compile-operation sexp))
    ((pred eltojs-is-defun?)
     (apply #'eltojs-compile-defun (cdr sexp)))
    ((pred eltojs-is-function-call?)
     (apply #'eltojs-compile-function-call sexp))
    ((pred numberp)
     (format "%s" sexp))
    ((pred stringp)
     (format "'%s'" (replace-regexp-in-string (rx "'") "\\'" sexp)))
    ((pred symbolp)
     (symbol-name sexp))
    (_ (error "Unknown case %S" sexp))))

(provide 'eltojs)

;;; eltojs.el ends here
