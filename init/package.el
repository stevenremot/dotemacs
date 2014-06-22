;;; package.el --- Package initialization

;;; Commentary:
;;

;;; Code:
(require 'package)

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

(provide 'init/package)

;;; package.el ends here
