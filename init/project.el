;;; project.el --- Projectile init module

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(use-package projectile
  :ensure projectile
  :init (projectile-global-mode 1))

(use-package helm-projectile
  :ensure helm-projectile)

(use-package magit
  :ensure magit)

(provide 'init/project)

;;; project.el ends here
