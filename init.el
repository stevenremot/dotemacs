;;; init.el --- Initialization file

;;; Commentary:

;;; Code:

;; Base configuration
;;;;;;;;;;;;;;;;;;;;;

(defvar personalconf--basedir (file-name-directory load-file-name))
(defvar personalconf--custom-lisp-dir (concat personalconf--basedir "/site-lisp"))
(defvar personalconf--custom-conf (concat personalconf--basedir "/custom-configuration.el"))

;; Custom configuration
;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file personalconf--custom-conf)
(load-file personalconf--custom-conf)

;; Base editing
;;;;;;;;;;;;;;;

(add-to-list 'load-path personalconf--custom-lisp-dir)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(show-paren-mode 1)

(column-number-mode 1)
(global-set-key (kbd "C-s-t") 'multi-term)
(global-set-key (kbd "C-s-s") 'speedbar)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'uniquify)

(ido-mode 1)

;; In order to make dead-circumflex work on emacs 24
(when (>= emacs-major-version 24)
  (require 'iso-transl))

;; Semantic mode
;;;;;;;;;;;;;;;;
(semantic-mode 1)
(defvar semantic-new-buffer-setup-functions)
(add-to-list 'semantic-new-buffer-setup-functions '(js2-mode . wisent-javascript-setup-parser))

;; ECB
;;;;;;

(global-set-key (kbd "C-s-p") 'ecb-activate)

;; Package repositories
;;;;;;;;;;;;;;;;;;;;;;;

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

;; Path variable
;;;;;;;;;;;;;;;;

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; Flymake / flycheck
;;;;;;;;;;;;;;;;;;;;;

(if (require 'flycheck nil :no-error)
    (add-hook 'after-init-hook #'global-flycheck-mode)
    (add-hook 'find-file-hook 'flymake-find-file-hook))

(global-set-key (kbd "C-M-d") 'flymake-display-err-menu-for-current-line)

;; tss
;;;;;;

(add-to-list 'load-path (concat personalconf--custom-lisp-dir "/emacs-tss"))
(require 'typescript)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(require 'tss)

;; Key bindings
(setq tss-popup-help-key "C-:")
(setq tss-jump-to-definition-key "C->")

(add-to-list 'tss-ac-trigger-command-keys "=")
(tss-config-default)

;;; init.el ends here
