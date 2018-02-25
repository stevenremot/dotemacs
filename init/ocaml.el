;;; ocaml.el --- Init ocaml support

;;; Commentary:
;;

;;; Code:

(defun init/setup-reason-buffer ()
  "Setup a buffer for sorking wioth reason."
  (add-hook 'before-save-hook #'refmt-before-save))



(use-package tuareg
  :ensure
  :mode ("\\.ml\\'" . tuareg-mode))

(use-package reason
  :ensure
  :hook ((reason-mode . init/setup-reason-buffer))
  :mode ("\\.re\\'" . reason-mode))

(use-package merlin
  :ensure
  :hook ((tuareg-mode . merlin-mode)))

(use-package lsp-ocaml
  :ensure
  :init
  (lsp-flycheck-add-mode 'tuareg-mode)
  (lsp-flycheck-add-mode 'reason-mode)
  :hook ((tuareg-mode . lsp-ocaml-enable)
	 (reason-mode . lsp-ocaml-enable)))

;;; ocaml.el ends here