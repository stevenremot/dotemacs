;;; project.el --- Projectile init module

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(defun init-project-add-projectile-hooks ()
  "Init projectile on supported modes.

Do not launch projectile in global mode because it doesn't play
nicely with TRAMP."
  (dolist (mode '(fundamental-mode-hook
                  prog-mode-hook
                  js3-mode-hook
                  conf-mode-hook
                  dired-mode-hook
                  org-mode-hook))
    (add-hook mode 'projectile-mode)))

(use-package projectile
  :ensure projectile
  :init (init-project-add-projectile-hooks)
  ;; Conflicts with svn, so using native cahing method for now.
  :config (progn (setq projectile-enable-caching t)))

(use-package helm-projectile
  :ensure helm-projectile)

(use-package magit
  :ensure magit)

(provide 'init/project)

;;; project.el ends here
