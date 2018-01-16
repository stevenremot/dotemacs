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
  (set-face-attribute 'default nil :height 140 :family "Source code pro"))



(use-package moe-theme
  :ensure
  :init (init/moe-theme))

;;; theme.el ends here
