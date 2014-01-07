;;; bootstrap.el --- Sets up the emacs environment

;;; Commentary:

;; This file is intended to setup a bare environment
;; It will install all required packages

;;; Code:

(require 'cl-lib)

;; Setup package repositories
(require 'package)

(when (< emacs-major-version 24)
  (add-to-list 'package-archives
               '("gnu" .
                 "http://elpa.gnu.org/packages/")))

(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" .
               "http://melpa.milkbox.net/packages/"))

(package-initialize)

(let* ((packages-standard (list
                           'exec-path-from-shell
                           'auto-complete
                           'mmm-mode
                           'js3-mode
                           'php-mode
                           'helm
                           ))

       (packages-emacs24 (list
                          'flycheck))

       (packages (append packages-standard packages-emacs24)))

  (cl-loop for p in packages
           unless (package-installed-p p)
           do (package-install p)))

;;; bootstrap.el ends here
