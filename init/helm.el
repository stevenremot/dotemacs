;;; helm.el --- Helm initialization module

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(use-package helm
  :init (progn
          (global-set-key (kbd "M-x") 'helm-M-x)
          (global-set-key (kbd "C-x C-f") 'helm-find-files)
          (global-set-key (kbd "C-x b") 'helm-buffers-list)
          (global-set-key (kbd "C-x C-b") 'helm-buffers-list)))

(provide 'init/helm)

;;; helm.el ends here
