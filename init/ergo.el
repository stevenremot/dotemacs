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

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(show-paren-mode 1)

(column-number-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; In order to make dead-circumflex work on emacs 24
(when (>= emacs-major-version 24)
  (require 'iso-transl))

(defvar my-confdir)

(defun my-switch-ctrl-caps-lock ()
  "Switch ctrl and caps lock keys on keyboard."
  (interactive)
  (shell-command (concat "xmodmap " my-confdir "xmodmap")))

;; Enabled features
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(provide 'init/ergo)

;;; ergo.el ends here
