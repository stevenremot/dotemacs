;;; web.el --- Web editing setup

;;; Commentary:
;;

;;; Code:

(use-package web-mode
  :ensure
  :mode "\\.html\\'")

(use-package rainbow-mode
  :ensure
  :hook (js-mode css-mode web-mode))

;;; web.el ends here
