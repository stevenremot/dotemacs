;;; javascript.el --- Setup javascript tools

;;; Commentary:
;;
;;; Code:
(require 'use-package)



(defun init/setup-javascript-lsp ()
  "Setup javascript for lsp."
  (autoload #'lsp-javascript-flow-enable "lsp-javascript-flow")
  (with-eval-after-load 'lsp-flycheck
    (lsp-flycheck-add-mode 'js-mode)
    (flycheck-add-next-checker 'javascript-eslint 'lsp)))

(defun init/configure-javascript-lsp ()
  "Configure LSP for the current buffer."
  (when (locate-dominating-file (buffer-file-name) ".flowconfig")
    (message "Setup flow LSP for %s" (buffer-file-name))
    (lsp-javascript-flow-enable))

  (setq flycheck-check-syntax-automatically '(save idle-change new-line mode-enabled))
  (local-set-key (kbd "M-.") #'xref-find-definitions))



(use-package lsp-javascript-typescript
  :ensure
  :after (js)
  :init (init/setup-javascript-lsp)
  :hook ((js-mode . init/configure-javascript-lsp)))

(use-package add-node-modules-path
  :ensure
  :hook ((js-mode . add-node-modules-path)))

(use-package json-mode
  :ensure)

;;; javascript.el ends here
