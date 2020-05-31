;;; auto-completion.el --- Setup code completion

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(use-package company
  :bind (("C-M-i" . company-complete))
  :config (global-company-mode))

;;; auto-completion.el ends here
