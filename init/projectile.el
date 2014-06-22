;;; projectile.el --- Projectile init module

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(use-package projectile
  :init (projectile-global-mode 1))

(provide 'init/projectile)

;;; projectile.el ends here
