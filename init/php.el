;;; php.el --- Setup PHP handling

;;; Commentary:
;;

;;; Code:

(use-package php-mode
  :ensure
  :mode "\\.php\\'")

(use-package lsp-php
  :ensure
  :hook ((php-mode . lsp-php-enable)))

(provide 'php)

;;; php.el ends here
