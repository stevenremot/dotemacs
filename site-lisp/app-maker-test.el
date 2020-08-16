;;; app-maker-test.el --- Tests for app-maker

;;; Commentary:
;;

(require 'ert)
(require 'app-maker)

;;; Code:

(cl-defstruct app-maker/test-counter (count 0))

(cl-defmethod am/render ((counter app-maker/test-counter))
  "Render COUNTER."
  `(p () ,(app-maker/test-counter-count counter)))

(ert-deftest app-maker/render-html-basic ()
  "It should render one HTML tag"
  (should (string= (am/render-html '(span (:style "color: red;") "Hello")) "<span style=\"color: red;\">Hello</span>")))

(ert-deftest app-maker/render-html-with-children ()
  "It should render HTML with children"
  (should (string= (am/render-html '(ul ()
					(li () "Hello")
					(li () "World")))
		   "<ul><li>Hello</li><li>World</li></ul>")))

(ert-deftest app-maker/render-html-with-renderable ()
  "It should render HTML with renderable items."
  (let ((counter (make-app-maker/test-counter :count 56)))
    (should (string= (am/render-html `(div () ,counter)) "<div><p>56</p></div>"))))

(provide 'app-maker-test)

;;; app-maker-test.el ends here

;; Local Variables:
;; after-save-hook: (lambda () (progn (eval-buffer) (ert-run-tests-interactively t)))
;; End:
