;;; theme.el --- Theme initialization

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(defun init/moe-theme ()
  "Setup moe theme."
  (require 'moe-theme)
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (menu-bar-mode 0)
  (moe-dark)
  (set-face-attribute 'default nil :height 140 :family "Source code pro")
  (with-eval-after-load 'ivy
    (set-face-attribute 'ivy-current-match nil :foreground "white smoke")))



(use-package moe-theme
  :ensure
  :init (init/moe-theme))

(use-package powerline
  :ensure
  :init
  (setq powerline-default-separator 'box)
  (powerline-default-theme))

(use-package dim
  :ensure
  :init
  (dim-minor-names
   '((projectile-mode "" projectile)
     (editorconfig-mode "" editorconfig)
     (company-mode "" company)
     (counsel-mode "" counsel)
     (ivy-mode "" ivy)
     (paredit-mode "" paredit)
     (page-break-lines-mode "" page-break-lines))))

;;; theme.el ends here
