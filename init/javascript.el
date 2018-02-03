;;; javascript.el --- Setup javascript tools

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(use-package add-node-modules-path
  :ensure
  :hook ((js-mode . add-node-modules-path)))

(use-package json-mode
  :ensure)

;;; javascript.el ends here
