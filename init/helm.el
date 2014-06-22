;;; helm.el --- Helm initialization module

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("C-x C-b" . helm-buffers-list)))

(provide 'init/helm)

;;; helm.el ends here
