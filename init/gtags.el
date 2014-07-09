;;; gtags.el --- GNU Global support initialization

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(use-package ggtags
  :init (add-hook 'text-mode-hook (lambda () (ggtags-mode 1))))

(provide 'init/gtags)

;;; gtags.el ends here
