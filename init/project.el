;;; project.el --- Projectile init module

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(use-package projectile
  :init (projectile-global-mode 1))

(use-package helm-projectile)

(use-package magit)

(provide 'init/project)

;;; project.el ends here
