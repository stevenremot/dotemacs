;;; editing.el --- Basic editing facilities

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(use-package page-break-lines
  :init (global-page-break-lines-mode))

(use-package editorconfig
  :if (locate-library "editorconfig")
  :hook (prog-mode . editorconfig-mode))



(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Rebind keys on mac os.
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta
	mac-right-option-modifier nil
	mac-command-modifier 'super))

(electric-pair-mode 1)
(show-paren-mode 1)

;;; editing.el ends here
