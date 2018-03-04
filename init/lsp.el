;;; lsp.el --- Setup the language-server-protocol basic tools

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(use-package lsp-mode
  :ensure
  :init
  (set-face-attribute 'lsp-face-highlight-write nil :background "dark green")
  )

(use-package lsp-ui
  :ensure
  :after (lsp-mode)
  :init
  (require 'lsp-ui-flycheck)
  (add-hook 'lsp-after-open-hook (lambda () (lsp-ui-flycheck-enable 1)))
  )

(use-package company-lsp
  :ensure
  :after (lsp-mode company)
  :init (add-to-list 'company-backends #'company-lsp))

;;; lsp.el ends here
