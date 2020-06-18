;;; rocktl.el --- Initialize rocktl

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(use-package rocktl
  :ensure nil
  :bind (("C-c & &" . rocktl-run-task)
	 ("C-c & s" . rocktl-status))
  :quelpa (rocktl :fetcher github :repo "stevenremot/emacs-rocktl"))

;;; rocktl.el ends here
