;;; eldoc.el --- Eldoc init module

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(use-package eldoc
  :init (progn
          (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
          (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)))

(use-package php-eldoc
  :init (add-hook 'php-mode-hook 'eldoc-mode))

(provide 'init/eldoc)

;;; eldoc.el ends here
