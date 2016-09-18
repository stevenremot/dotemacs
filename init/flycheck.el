;;; flycheck.el --- Flychek init

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(use-package flycheck
  :ensure
  :init (global-flycheck-mode))

(provide 'init/flycheck)

;;; flycheck.el ends here
