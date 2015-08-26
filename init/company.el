;;; company.el --- Company init module

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(use-package company
  :ensure company
  :init (add-hook 'after-init-hook 'global-company-mode))

(use-package company-tern
  :ensure company-tern
  :init (add-to-list 'company-backends 'company-tern))

(provide 'init/company)

;;; company.el ends here
