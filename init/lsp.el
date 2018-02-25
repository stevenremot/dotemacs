;;; lsp.el --- Setup the language-server-protocol basic tools

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(use-package lsp-mode
  :ensure
  :init
  (require 'lsp-flycheck)
  (set-face-attribute 'lsp-face-highlight-write nil :background "dark green")
  )

(use-package company-lsp
  :ensure
  :after (lsp company)
  :init (add-to-list 'company-backends #'company-lsp))

;;; lsp.el ends here
