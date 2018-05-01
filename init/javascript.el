;;; javascript.el --- Setup javascript tools

;;; Commentary:
;;
;;; Code:
(require 'use-package)



(defun init/setup-javascript-lsp ()
  "Setup javascript for lsp."
  (autoload #'lsp-javascript-flow-enable "lsp-javascript-flow")
  (autoload #'lsp-javascript-typescript-enable "lsp-javascript-typescript")

  (with-eval-after-load 'lsp-ui-flycheck
    (lsp-ui-flycheck-add-mode 'js-mode)
    (flycheck-add-next-checker 'lsp-ui 'javascript-eslint)))

(defun init/configure-javascript-lsp ()
  "Configure LSP for the current buffer."
  (if (locate-dominating-file (buffer-file-name) ".flowconfig")
      (progn
	(message "Setup flow LSP for %s" (buffer-file-name))
	(lsp-javascript-flow-enable))
    (message "Setup js/ts LSP for %s" (buffer-file-name))
    (lsp-javascript-typescript-enable))

  (setq flycheck-check-syntax-automatically '(save idle-change new-line mode-enabled))
  (local-set-key (kbd "M-.") #'xref-find-definitions))



(use-package typescript-mode
  :ensure
  :mode "\\.ts\\'")

(use-package lsp-javascript-typescript
  :ensure
  :after (js)
  :init (init/setup-javascript-lsp)
  :hook ((js-mode . init/configure-javascript-lsp)
	 (typescript-mode . init/configure-javascript-lsp)))

(use-package add-node-modules-path
  :ensure
  :hook ((js-mode . add-node-modules-path)
	 (typescript-mode . add-node-modules-path)))

(use-package json-mode
  :ensure)

;;; javascript.el ends here
