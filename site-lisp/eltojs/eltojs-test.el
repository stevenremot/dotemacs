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

(provide 'eltojs-test)

;;; eltojs-test.el ends here
