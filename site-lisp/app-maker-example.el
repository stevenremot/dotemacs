;;; app-maker-example.el --- Example of an app-maker app -*- lexical-binding: t -*-

;;; Commentary:
;;

(require 'app-maker)
(require 'app-maker-material)
(require 'cl-lib)

;;; Code:

(cl-defstruct counter-app (count 0))

(defun counter-app-update (counter offset)
  "Increment / decrament the COUNTER with OFFSET."
  (setf (counter-app-count counter) (+ offset (counter-app-count counter))))

(cl-defmethod am/render ((counter counter-app))
  "Render COUNTER with + and - buttons."
  (amui/page
   '(:title "Counter example")

   (amui/flex
    (list
     :dir "row"
     :align "center"
     :justify "center"
     :style "height: 100%;")

    (amui/flex
     (list
      :dir "row"
      :align "center"
      :gap "16px")

     (amui/button (list
		   :on-click #'(lambda () (counter-app-update counter -1))
		   :style "primary")
		  "-")

     (amui/text (list :style "body1") (counter-app-count counter))

     (amui/button (list
		   :on-click #'(lambda () (counter-app-update counter 1))
		   :style "primary")
		  "+")))
    ))

(am/defapp test/counter
  (make-counter-app))

(provide 'app-maker-example)

;;; app-maker-example.el ends here
