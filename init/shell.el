;;; shell.el --- Shell init module

;;; Commentary:
;;
(require 'use-package)
;;; Code:

(defvar eshell-path-env)
(defvar exec-path)

(defconst my-added-path "")

(eval-after-load 'eshell
  '(setq eshell-path-env (concat eshell-path-env ":" my-added-path)))

;; Required for emacs lisp flycheck to work
(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

(add-to-list 'exec-path my-added-path)


(provide 'init/shell)

;;; shell.el ends here
