;;; auto-completion.el --- Setup code completion

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(use-package company
  :ensure
  :bind (("C-M-i" . company-complete))
  :init (global-company-mode))

;;; auto-completion.el ends here
