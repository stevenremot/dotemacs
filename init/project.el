;;; project.el --- Setup project management tools

;;; Commentary:
;; 
;;; Code:
(require 'use-package)

(use-package projectile
  :ensure
  :init (projectile-global-mode))

(use-package counsel-projectile
  :ensure
  :after (projectile ivy)
  :init (counsel-projectile-mode))

;;; project.el ends here
