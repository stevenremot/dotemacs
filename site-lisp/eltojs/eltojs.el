;;; eltojs.el --- Simple Emacs Lisp to JS compiler -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'seq)

(cl-defstruct eltojs-compile-rule predicate compiler (priority 2))

(defvar eltojs--compile-rules '())

(cl-defun eltojs-add-compile-rule (&key predicate compiler(priority 2))
  "Define a new compile rule.

PREDICATE, COMPILER and PRIORITY are struct fields."
  (setq eltojs--compile-rules
	(sort
	 (push (make-eltojs-compile-rule
		:predicate predicate
		:compiler compiler
		:priority priority)
	       eltojs--compile-rules)
	 #'(lambda (rule-1 rule-2)
	     (< (eltojs-compile-rule-priority rule-1)
		(eltojs-compile-rule-priority rule-2))))))

(cl-defun eltojs-add-simple-rule (symbol compiler &key (priority 1))
  (eltojs-add-compile-rule
 :predicate #'(lambda (sexp)
		(and (listp sexp) (eq symbol (car sexp))))
 :compiler #'(lambda (sexp)
	       (apply compiler (cdr sexp)))
 :priority priority))

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


;; Raw el
(eltojs-add-simple-rule
 '$el
 (lambda (&rest args)
   (dolist (expr args)
     (eval expr))))

;; Raw JS
(eltojs-add-simple-rule
 '$js
 (lambda (&rest args)
   (mapconcat #'identity args ";\n")))

(defun eltojs-generate-arrow-function (args body)
  "Write an arrow function wuth the given ARGS and BODY."
  (format "(%s) => {\n%s\n}"
	  (eltojs-compile-comma-list args)
	  (eltojs-compile-function-body body)))

;; Lambda
(eltojs-add-compile-rule
 :predicate #'(lambda (sexp)
		(and (listp sexp) (eq 'function (car sexp))))
 :compiler #'(lambda (sexp)
	       (let* ((parts (cdr (cadr sexp)))
		      (args (car parts))
		      (body (cdr parts)))
		 (eltojs-generate-arrow-function args body)))
 :priority 1)

;; Lambda - lexical binding
(eltojs-add-compile-rule
 :predicate #'(lambda (sexp)
		(and (listp sexp) (eq 'closure (car sexp))))
 :compiler #'(lambda (sexp)
	       (let* ((parts (cddr sexp))
		      (args (car parts))
		      (body (cdr parts)))
		 (eltojs-generate-arrow-function args body)))
 :priority 1)

(eltojs-add-simple-rule
 'defalias
 (lambda (name value)
   (format "const %s = %s"
	   (symbol-name (eval name))
	   (eltojs-compile value))))

(eltojs-add-simple-rule
 '1+
 (lambda (n)
   (format "1 + (%s)"
	   (eltojs-compile n))))


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

(defun eltojs-compile-function-body (body)
  (let ((last-index (1- (seq-length body))))
    (thread-last body
      (seq-map-indexed (lambda (line index)
			 (format
			  (if (< index last-index)
			      "  %s;\n"
			    "  return %s;")
			  (eltojs-compile line))))
      (string-join))))

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
     (eltojs-compile-function-body
      (append (list docstring-or-body) body))))
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

; Legacy rule
(eltojs-add-compile-rule
 :predicate #'(lambda (sexp) t)
 :compiler #'(lambda (sexp)
  (let ((expanded-sexp sexp))
    (pcase expanded-sexp
      ((pred eltojs-is-arithmetic-operation?)
       (apply #'eltojs-compile-operation expanded-sexp))
      ((pred eltojs-is-defun?)
       (apply #'eltojs-compile-defun (cdr expanded-sexp)))
      ((pred eltojs-is-function-call?)
       (apply #'eltojs-compile-function-call expanded-sexp))
      ((pred numberp)
       (format "%s" expanded-sexp))
      ((pred stringp)
       (format "'%s'" (replace-regexp-in-string (rx "'") "\\'" expanded-sexp)))
      ((pred symbolp)
       (symbol-name expanded-sexp))
      (_ (error "Unkown case %S" expanded-sexp))))))

(defun eltojs-compile (sexp)
  "Compile SEXP to javascript."
  (let ((expanded-sexp (macroexpand-all sexp)))
    (catch 'result
      (dolist (rule eltojs--compile-rules)
	(when (funcall (eltojs-compile-rule-predicate rule) expanded-sexp)
	  (throw 'result
		 (funcall (eltojs-compile-rule-compiler rule) expanded-sexp))))
      (error "No rule for sexp %S" expanded-sexp))))

(defun eltojs-compile-buffer (output-file)
  "Translate the current buffer to js and savee the result to OUTPUT-FILE."
  (interactive "F")
  (let ((code ""))
    (save-excursion
      (goto-char (point-min))
      (condition-case failure
	  (while (not (eq (point-max) (point)))

	    (setq code (if-let* ((line-code (eltojs-compile (read (current-buffer))))
				 (has-code? (not (string= "" line-code))))
			   (if (string= "" code)
			       line-code
			     (concat code ";\n" line-code))
			 code)))
        (end-of-file nil)))


    (with-temp-buffer
      (insert code)
      (write-region nil nil output-file))))

(defun eltojs-compile-file (source-file output-file)
  "Compile a SOURCE-FILE in elisp to JS in OUTPUT-FILE."
  (with-temp-buffer
    (insert-file-contents source-file)
    (eltojs-compile-buffer output-file)))

(provide 'eltojs)

;;; eltojs.el ends here
