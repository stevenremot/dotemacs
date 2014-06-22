;;; typescript.el --- Typescript init module

;;; Commentary:
;;
;;; Code:
(defvar my-site-lisp)
(add-to-list 'load-path (concat my-site-lisp "/emacs-tss"))

(when (require 'tss nil :noerror)
  (defvar tss-popup-help-key)
  (defvar tss-jump-to-definition-key)
  (defvar tss-ac-trigger-command-keys)


  (require 'typescript)
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

  ;; Key bindings
  (setq tss-popup-help-key "C-:")
  (setq tss-jump-to-definition-key "C->")

  (add-to-list 'tss-ac-trigger-command-keys "=")
  (tss-config-default))

(provide 'init/typescript)

;;; typescript.el ends here
