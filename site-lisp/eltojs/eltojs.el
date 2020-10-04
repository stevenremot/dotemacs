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

(defun eltojs-compile-function-call (symbol &rest args)
  "Compile an expression calling SYMBOL on ARGS."
  (format "%s(%s)" symbol (mapconcat #'eltojs-compile args ", ")))

(defun eltojs-compile (sexp)
  "Compile SEXP in javascript."
  (pcase sexp
    ((pred eltojs-is-arithmetic-operation?)
     (apply #'eltojs-compile-operation sexp))
    ((pred eltojs-is-function-call?)
     (apply #'eltojs-compile-function-call sexp))
    ((pred numberp)
     (format "%s" sexp))
    ((pred stringp)
     (format "'%s'" (replace-regexp-in-string (rx "'") "\\'" sexp)))
    (_ (error "Unknown case %S" sexp))))

(provide 'eltojs)

;;; eltojs.el ends here
