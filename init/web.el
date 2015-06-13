;;; web.el --- Web mode init module

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(use-package web-mode
  :ensure web-mode
  :mode "\\.p?html\\'"
  :config (progn
            (setq web-mode-engines-alist
                  '(("php" . "\\.phtml\\'")))
            (add-hook 'web-mode-hook (lambda ()
                                            (emmet-mode 1)
                                            (skewer-mode 1)))))

(use-package js2-mode
  :ensure js2-mode
  :config (progn
            (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
            (add-hook 'js2-mode-hook 'skewer-mode)))

(use-package js2-refactor
  :ensure js2-refactor
  :config (js2r-add-keybindings-with-prefix "C-S-r"))

(use-package css-mode
  :config (add-hook 'css-mode-hook 'skewer-mode))

(use-package skewer-mode
  :ensure skewer-mode)

(defun my-web-kill-skewer-snippet ()
  "Put the snippet to make a webpage skewer-aware in the clipboard."
  (interactive)
  (kill-new "(function () {
    var s = document.createElement(\"script\");
    s.src = \"//localhost:8080/skewer\";
    document.getElementsByTagName(\"head\")[0].appendChild(s);
})()"))

;; Tern
(use-package tern
  :ensure tern
  :init (add-hook 'js2-mode-hook (lambda () (tern-mode t))))

(use-package company-tern
  :ensure company-tern
  :init (progn
          (add-to-list 'company-backends 'company-tern)
          (add-hook 'js2-mode-hook (lambda () (company-tern t)))))

;; Emmet
(use-package emmet-mode
  :ensure emmet-mode
  :bind (("C-c C-c RET" . emmet-expand-line)
         ("S-<left>" . emmet-prev-edit-point)
         ("S-<right>" . emmet-next-edit-point)))

(defvar my-site-lisp)
(add-to-list 'load-path (concat my-site-lisp "gulpjs/"))
(autoload 'gulpjs-start-task "gulpjs" "Start a gulp task." t)

;; Geben
(fset 'my-open-file-geben
   [?\M-x ?m ?y ?  ?k ?i ?l ?l ?  ?f ?i ?l ?e ?  ?n ?a ?m ?e return ?\C-x ?o ?\C-c ?f ?\C-a ?\C-k ?\C-v ?\M-y return])

(use-package rainbow-mode
  :init (progn (add-hook 'css-mode-hook 'rainbow-mode)
               (add-hook 'less-css-mode-hook 'rainbow-mode)))

(provide 'init/web)

;;; web.el ends here
