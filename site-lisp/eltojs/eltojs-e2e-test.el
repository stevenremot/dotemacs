($el
  (require 'eltojs-lib))

(js-import test "node:test")
(js-import assert "node:assert/strict")

(test "it compiles arithmetic operations"
      (lambda ()
	(assert.equal (+ 2 3) 5)
	(assert.equal (- 2 3) -1)
	(assert.equal (* 2 3) 6)
	(assert.equal (/ 3 2) 1.5)))

(test "it does arithmetic with the right precedence"
      (lambda ()
	(assert.equal (* 3 (- 7 5)) 6)))

(test "it supports defining and calling functions"
      (lambda ()
	(defun increment (n)
	  (1+ n))

	(assert.equal (increment 7) 8)))
