;;; javascript.el --- Setup javascript tools

;;; Commentary:
;; 
;;; Code:
(require 'use-package)

(use-package add-node-modules-path
  :ensure
  :hook ((js-mode . add-node-modules-path)))

;;; javascript.el ends here
