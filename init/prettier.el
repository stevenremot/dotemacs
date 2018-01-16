;;; prettier.el --- Setup prettier

;;; Commentary:
;; 
;;; Code:
(require 'use-package)

(use-package prettier-eslint
  :bind (("C-c f" . prettier-eslint)))

;;; prettier.el ends here
