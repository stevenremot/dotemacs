;;; bootstrap.el --- Sets up the emacs environment

;;; Commentary:

;; This file is intended to setup specialized environments.
;; An environment is a list of packages.

;; The script handles dependencies between environment (not circular ones though)

;; To install an environment, just do
;; (bootstrap-install <environment>)

;; For example, to install the web development packages, one would do
;; (bootstrap-install 'web)

;; The environments currently defined are :
;; * basic - General editing packages
;; * basic-emacs24 - General packages intended to be used at least on Emacs 24
;; * mailing - Basic mail utilities (actually, prefer mu4e)
;; * project - Packages for project management (projectile)
;; * web - Packages for web development
;; * web-emacs24 - Packages for web development intended to be used at least on Emacs 24

;;; Code:

(require 'cl-lib)
(require 'package)

(defvar bootstrap-environment-packages
  '((basic . (exec-path-from-shell
              company
              eldoc
              solarized-theme
              powerline
              ))
    (basic-emacs24 . (flycheck
                      ))
    (mailing . (bbdb))
    (project . (projectile
                magit
                ))
    (web . (web-mode
            js3-mode
            php-mode
            php-eldoc
            ))
    (web-emacs24 . (tern
                    company-tern
                    ))
    (lisp . (paredit))
    (clojure . (clojure-mode
                cider
                clojurescript-mode))
    )
  "Association between desired environment and packages.")

(defvar bootstrap-environment-dependency
  '((basic . ())
    (basic-emacs24 . (basic))
    (project . (basic))
    (web . (basic))
    (web-emacs24 . (basic-emacs24 web))
    (clojure . (basic lisp)))
  "Define dependencies for each environment.")

(defun bootstrap--init-package ()
  "Set up package sources."
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

  (package-initialize))

(defun bootstrap-get-environment-dependency-list (environment)
  "Return a list of the ENVIRONMENT's dependencies."
  (let ((direct-dependencies (cdr (assoc environment bootstrap-environment-dependency)))
        (dependencies '()))
    (when direct-dependencies
      (dolist (dependency direct-dependencies)
        (dolist (subdependency (bootstrap-get-environment-dependency-list dependency))
          (add-to-list 'dependencies subdependency))))
    (add-to-list 'dependencies environment)
    dependencies))

(defun bootstrap-get-environment-packages (environment)
  "Return a list of the packages that must be installed for this ENVIRONMENT.
Include the dependencies."
  (let ((packages '())
        (envs (bootstrap-get-environment-dependency-list environment)))
    (dolist (env envs)
      (dolist (package (reverse (cdr (assoc env bootstrap-environment-packages))))
        (add-to-list 'packages package)))
    packages))

(defun bootstrap-install (environment)
  "Install all packages for the ENVIRONMENT."
  (bootstrap--init-package)
  (cl-loop for package in (bootstrap-get-environment-packages environment)
           unless (package-installed-p package)
           do (package-install package)))

(provide 'bootstrap)

;;; bootstrap.el ends here
