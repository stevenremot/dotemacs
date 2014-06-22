;;; company.el --- Company init module

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode))

(use-package company-tern
  :init (add-to-list 'company-backends 'company-tern))

(use-package helm-company
  :init (global-set-key (kbd "C-:") 'helm-company))

(provide 'init/company)

;;; company.el ends here
