;;; lisp.el --- Lisp init module

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(defvar flycheck-emacs-lisp-load-path)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq flycheck-emacs-lisp-load-path load-path)))

(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)

(use-package paredit
  :ensure paredit
  :init (progn
          (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
          (add-hook 'lisp-interaction-mode-hook 'paredit-mode)
          (add-hook 'clojure-mode-hook 'paredit-mode)))

(use-package nameless
  :ensure
  :init (add-hook 'emacs-lisp-mode-hook #'nameless-mode))

(provide 'init/lisp)

;;; lisp.el ends here
