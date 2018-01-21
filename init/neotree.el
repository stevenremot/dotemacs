;;; neotree.el --- Neotree setup

;;; Commentary:
;;
(require 'use-package)

;;; Code:

(use-package all-the-icons
  :ensure)

(use-package neotree
  :ensure
  :custom
  (neo-theme 'icons))

;;; neotree.el ends here
