;;; gtags.el --- GNU Global support initialization

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(defun my-enable-ggtags ()
  "Enable ggtags mode on different hooks."
  (dolist (mode '(text-mode php-mode web-mode))
    (add-hook (intern (concat (symbol-name mode) "-hook"))
              (lambda () (ggtags-mode 1)))))

(use-package ggtags
  :init (my-enable-ggtags))

(provide 'init/gtags)

;;; gtags.el ends here
