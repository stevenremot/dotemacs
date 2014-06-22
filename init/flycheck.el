;;; flycheck.el --- Flychek init

;;; Commentary:
;;
;;; Code:
(require 'use-package)

;; Required for emacs lisp flycheck to work
(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

(use-package flycheck
  :init (global-flycheck-mode))

(provide 'init/flycheck)

;;; flycheck.el ends here
