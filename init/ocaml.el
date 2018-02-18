;;; ocaml.el --- Init ocaml support

;;; Commentary:
;;

;;; Code:

(use-package tuareg
  :ensure
  :mode ("\\.ml\\'" . tuareg-mode))

(use-package merlin
  :ensure
  :hook ((tuareg-mode . merlin-mode)))

;; (use-package lsp-ocaml
;;   :ensure
;;   :init
;;   (lsp-flycheck-add-mode 'tuareg-mode)
;;   :hook ((tuareg-mode . lsp-ocaml-enable)))

;;; ocaml.el ends here
