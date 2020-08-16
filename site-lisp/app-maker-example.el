;;; app-maker-example.el --- Example of an app-maker app -*- lexical-binding: t -*-

;;; Commentary:
;;

(require 'app-maker)
(require 'cl-lib)

;;; Code:

(cl-defstruct counter-app (count 0))

(defun counter-app-update (counter offset)
  "Increment / decrament the COUNTER with OFFSET."
  (setf (counter-app-count counter) (+ offset (counter-app-count counter))))

(cl-defmethod am/render ((counter counter-app))
  "Render COUNTER with + and - buttons."
  `(div
    ()
    (a (:href ,(lambda () (counter-app-update counter -1))) "-")
    (span () ,(counter-app-count counter))
    (a (:href ,(lambda () (counter-app-update counter 1))) "+")))

(am/defapp test/counter
  (make-counter-app))

(provide 'app-maker-example)

;;; app-maker-example.el ends here
