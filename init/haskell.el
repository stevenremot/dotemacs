;;; haskell.el --- Haskell init module

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(use-package haskell-mode
  :ensure haskell-mode
  :config (add-hook 'haskell-mode-hook 'haskell-indentation-mode))

(provide 'init/haskell)

;;; haskell.el ends here
