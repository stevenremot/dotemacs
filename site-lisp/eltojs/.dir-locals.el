;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((emacs-lisp-mode . ((after-save-hook . (lambda nil
					  (eval-buffer)
					  (ert "eltojs-"))))))
