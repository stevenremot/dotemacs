;;; lsp-reason.el --- Support for LSP in reason buffers

;;; Commentary:
;;
(require 'lsp-mode)
;;; Code:

(defcustom lsp-reason-program "reason-language-server.exe"
  "Location of the reason language server binary.")

;;;###autoload
(lsp-define-stdio-client
      lsp-reason
      "reason"
      (lambda () (locate-dominating-file buffer-file-name "bsconfig.json"))
      (list lsp-reason-program))

(provide 'lsp-reason)

;;; lsp-reason.el ends here
