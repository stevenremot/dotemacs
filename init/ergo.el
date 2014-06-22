;;; ergo.el --- UX initialization module

;;; Commentary:
;;
;;; Code:

(global-set-key (kbd "C-s-t") (lambda () (interactive) (eshell t)))
(global-set-key (kbd "C-s-s") 'speedbar)

(global-set-key (kbd "C-z") 'undo)

(require 'uniquify)
(when window-system
  (tool-bar-mode -1)
  (menu-bar-mode -1))
(ido-mode 1)

(provide 'init/ergo)

;;; ergo.el ends here
