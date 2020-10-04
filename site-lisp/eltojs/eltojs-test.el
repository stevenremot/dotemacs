;;; eltojs-test.el --- eltojs tests

;;; Commentary:
;;

(require 'ert)

;;; Code:

(defmacro eltojs-should-compile (sexp js)
  "Check a SEXP compiles to JS."
  `(should (equal (eltojs-compile (quote ,sexp)) ,js)))

(ert-deftest eltojs-compile-basic-arihtmetic ()
  "Compiles basic arithmetic expressions"
  (eltojs-should-compile
   (+ 1 2)
   "1 + 2")

  (eltojs-should-compile
   (- 1 5 6)
   "1 - 5 - 6")

  (eltojs-should-compile
   (* 2 (+ 4 9))
   "2 * (4 + 9)"))

(ert-deftest eltojs-compile-function-calls ()
  "Compiles function calls"
  (eltojs-should-compile
   (alert "Hello world")
   "alert('Hello world')"))

(ert-deftest eltojs-compile-method-calls ()
  "Compiles method calls"
  (eltojs-should-compile
   (.log console "Hello world")
   "console.log('Hello world')"))

(ert-deftest eltojs-compile-function-definitions ()
  "Compiles function definitions"
  (eltojs-should-compile
   (defun greet (name)
     "Greets"
     (alert "Hello")
     (alert name))
   "/**
 * Greets
 */
function greet(name) {
  alert('Hello');
  alert(name);
}")

  (eltojs-should-compile
   (defun greetNoComment (name)
     (alert "Hello")
     (alert name))
   "function greetNoComment(name) {
  alert('Hello');
  alert(name);
}"))

(provide 'eltojs-test)

;;; eltojs-test.el ends here
