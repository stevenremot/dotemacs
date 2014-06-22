;;; shortcuts.el --- Shortcuts initialization module

;;; Commentary:
;;
;;; Code:

(global-set-key (kbd "C-s-t") (lambda () (interactive) (eshell t)))
(global-set-key (kbd "C-s-s") 'speedbar)

(global-set-key (kbd "C-z") 'undo)

(provide 'init/shortcuts)

;;; shortcuts.el ends here
