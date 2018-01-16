;;; exec-path-from-shell.el --- Copy the PATH variable from the shell to Emacs

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(use-package exec-path-from-shell
  :ensure
  :init (exec-path-from-shell-initialize))

;;; exec-path-from-shell.el ends here
