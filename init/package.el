;;; package.el --- Package initialization

;;; Commentary:
;;

;;; Code:
(require 'package)

(fset 'my-show-installed-packages
      (lambda (&optional arg)
        "Show installed packages."
        (interactive "p")
        (kmacro-exec-ring-item
         (quote ([134217843 111 dead-circumflex 32 73 return] 0 "%d")) arg)))

(eval-after-load 'package
  '(define-key package-menu-mode-map
     (kbd "C-c s")
     'perso-show-installed-packages))

(eval-after-load 'package
  '(define-key package-menu-mode-map
     (kbd "C-c s")
     'perso-show-installed-packages))

(when (< emacs-major-version 24)
  (add-to-list 'package-archives
               '("gnu" .
                 "http://elpa.gnu.org/packages/")))

(add-to-list 'package-archives
             '("melpa" .
               "http://melpa.milkbox.net/packages/"))

(add-to-list 'package-archives
             '("stable-melpa" .
               "http://melpa-stable.milkbox.net/packages/"))

(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(provide 'init/package)

;;; package.el ends here
