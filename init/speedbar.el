;;; speedbar.el --- Speedbar init module

;;; Commentary:
;;
(require 'use-package)
;;; Code:

(use-package sr-speedbar
  :ensure sr-speedbar
  :bind (("C-c s s" . sr-speedbar-toggle)))

(provide 'init/speedbar)

;;; speedbar.el ends here
