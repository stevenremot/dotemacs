;;; editing.el --- Basic editing facilities

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(use-package page-break-lines
  :ensure
  :init (global-page-break-lines-mode))

(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Rebind keys on mac os.
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta
	mac-right-option-modifier nil
	mac-command-modifier 'super))

(electric-pair-mode 1)

;;; editing.el ends here
