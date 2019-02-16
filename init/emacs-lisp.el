;;; emacs-lisp.el --- Setup emacs-lisp editing facilities

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(defun init/setup-elisp-mode ()
  "Setup emacs lisp mode."
  (define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer))



(use-package elisp-mode
  :ensure nil
  :config (init/setup-elisp-mode))

(use-package paredit
  :hook (emacs-lisp-mode . paredit-mode)
  :ensure)

;;; emacs-lisp.el ends here
