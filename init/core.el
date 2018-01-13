;;; core.el --- Core setup requires for the initialization

;;; Commentary:
;;
;;; Code:
(require 'package)

(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(add-to-list 'package-archives (cons "melpa-stable" "https://stable.melpa.org/packages/") t)
(package-initialize)

(let ((packages-to-install '(use-package quelpa-use-package)))
  (dolist (package packages-to-install)
    (unless (package-installed-p package)
      (package-refresh-contents)
      (package-install package))))

;;; core.el ends here
