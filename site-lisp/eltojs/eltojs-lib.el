;;; eltojs-lib.el --- Utility functions for eltojs scripts

;;; Commentary:
;;

(defmacro js-import (imports path)
  "Generate an ES2015 import."
  `($js ,(format "import %s from %S" imports path)))

(provide 'eltojs-lib)

;;; eltojs-lib.el ends here
