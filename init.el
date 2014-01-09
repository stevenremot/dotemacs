;;; init.el --- Initialization file

;;; Commentary:

;;; Code:

;; Base configuration
;;;;;;;;;;;;;;;;;;;;;

(defvar personalconf--basedir (file-name-directory load-file-name))
(defvar personalconf--custom-lisp-dir (concat personalconf--basedir "/site-lisp"))
(defvar personalconf--custom-conf (concat personalconf--basedir "/custom-configuration.el"))
(defvar personalconf--local-init-file (concat personalconf--basedir "/init-local.el"))

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
(tool-bar-mode -1)

;; In order to make dead-circumflex work on emacs 24
(when (>= emacs-major-version 24)
  (require 'iso-transl))

;; Semantic mode
;;;;;;;;;;;;;;;;
(semantic-mode 1)
(defvar semantic-new-buffer-setup-functions)

(add-to-list 'semantic-new-buffer-setup-functions '(js3-mode . wisent-javascript-setup-parser))
(global-set-key [(control return)] 'semantic-ia-complete-symbol)



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
(add-hook 'php-mode-hook 'flymake-phpmd-setup)

;; auto-complete
;;;;;;;;;;;;;;;;;

(require 'auto-complete)
(add-to-list 'ac-modes 'php-mode)
(global-auto-complete-mode t)

;; tss
;;;;;;

(add-to-list 'load-path (concat personalconf--custom-lisp-dir "/emacs-tss"))

(when (require 'tss nil :noerror)
  (defvar tss-popup-help-key)
  (defvar tss-jump-to-definition-key)
  (defvar tss-ac-trigger-command-keys)

  (require 'typescript)
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

  ;; Key bindings
  (setq tss-popup-help-key "C-:")
  (setq tss-jump-to-definition-key "C->")

  (add-to-list 'tss-ac-trigger-command-keys "=")
  (tss-config-default))

;; mmm-mode
;;;;;;;;;;;

(require 'mmm-auto)
(defvar mmm-global-mode)
(setq mmm-global-mode :auto)

(add-to-list 'mmm-major-mode-preferences '(html-js . js3-mode))
(mmm-add-mode-ext-class 'html-mode nil 'html-js)
(mmm-add-mode-ext-class 'html-mode nil 'html-css)
(mmm-add-mode-ext-class 'html-mode nil 'html-php)


(add-to-list 'auto-mode-alist '(".html\\.phtml\\'" . html-mode))

;; Helm
;;;;;;;

(when (require 'helm nil :no-error)
  (helm-mode 1))

;; Projectile
;;;;;;;;;;;;;


(when (require 'projectile nil :no-error)
  (projectile-global-mode 1))

;; Local configuration
;;;;;;;;;;;;;;;;;;;;;;;

(when (file-exists-p personalconf--local-init-file)
  (load-file personalconf--local-init-file))


;;; init.el ends here
