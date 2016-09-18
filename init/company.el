;;; company.el --- Company init module

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(use-package company
  :ensure
  :init (add-hook 'after-init-hook 'global-company-mode))

(use-package company-tern
  :ensure
  :init (add-to-list 'company-backends 'company-tern))

(provide 'init/company)

;;; company.el ends here
