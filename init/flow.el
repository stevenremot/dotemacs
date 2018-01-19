;;; flow.el --- Setup tools for working with flow typings

;;; Commentary:
;;
;;; Code:
(require 'use-package)
(require 'quelpa-use-package)

(defun init/setup-lsp-javascript-flow ()
  "Setup lsp-javascript-flow package."
  (with-eval-after-load 'lsp-flycheck
    (lsp-flycheck-add-mode 'js-mode)
    (flycheck-add-next-checker 'javascript-eslint 'lsp)))

(defun init/javascript-flow-configure ()
  "Configure JS buffers."
  (lsp-javascript-flow-enable)
  (setq flycheck-checker 'javascript-eslint)
  (local-set-key (kbd "M-.") #'xref-find-definitions))



(use-package lsp-javascript-flow
  :ensure
  :quelpa (lsp-javascript-flow :fetcher github :repo "stevenremot/emacs-lsp-javascript-flow")
  :hook (js-mode . init/javascript-flow-configure)
  :commands (lsp-javascript-flow-enable)
  :init (init/setup-lsp-javascript-flow))

;;; flow.el ends here
