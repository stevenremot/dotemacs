;;; code-checking.el --- Setup general code checkers

;;; Commentary:
;;
;;; Code:

(require 'use-package)

(use-package flycheck
  :ensure
  :config (global-flycheck-mode))

;;; code-checking.el ends here
