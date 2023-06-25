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
