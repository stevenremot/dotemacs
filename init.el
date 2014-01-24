;;; init.el --- Initialization file

;;; Commentary:

;;; Code:

;; Base configuration
;;;;;;;;;;;;;;;;;;;;;

(defvar personalconf--basedir (file-name-directory load-file-name))
(defvar personalconf--custom-lisp-dir (concat personalconf--basedir "site-lisp/"))
(defvar personalconf--custom-conf (concat personalconf--basedir "custom-configuration.el"))
(defvar personalconf--local-init-file (concat personalconf--basedir "init-local.el"))
(defvar personalconf--authinfo-file (concat personalconf--basedir ".authinfo.gpg"))

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
(tool-bar-mode -1)
(ido-mode 1)

;; In order to make dead-circumflex work on emacs 24
(when (>= emacs-major-version 24)
  (require 'iso-transl))

;; Semantic mode
;;;;;;;;;;;;;;;;
(semantic-mode 1)
(defvar semantic-new-buffer-setup-functions)

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
(add-to-list 'ac-modes 'js3-mode)
(global-auto-complete-mode t)

;; Projectile
;;;;;;;;;;;;;

(when (require 'projectile nil :no-error)
  (projectile-global-mode 1))

;; Tern
;;;;;;;

(add-hook 'js3-mode-hook (lambda ()
                           (when (require 'tern nil :no-error)
                             (tern-mode t))))

(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

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

;; Gnus
;;;;;;;;
(require 'epa-file)
(epa-file-enable)

(eval-after-load 'gnus '(progn
                          (defvar gnus-select-method)
                          (defvar gnus-secondary-select-methods)

                          (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

                          (setq gnus-select-method '(nnrss "http://planet.lisp.org/rss20.xml"))
                          (setq gnus-secondary-select-methods `((nnimap "perso"
                                                                        (nnimap-address "imap.gmail.com")
                                                                        (nnimap-authinfo-file ,personalconf--authinfo-file))
                                                                (nnimap "telecom"
                                                                        (nnimap-address "z.mines-telecom.fr")
                                                                        (nnimap-authinfo-file ,personalconf--authinfo-file))
                                                                (nnimap "inovia"
                                                                        (nnimap-address "imap.gmail.com")
                                                                        (nnimap-authinfo-file ,personalconf--authinfo-file))
                                                                (nnrss "http://planet.lisp.org/rss20.xml")
                                                                (nnrss "http://celeron.55.lt/blog/?feed=rss2")))))

;; BBDB
;;;;;;;

(when (require 'bbdb nil :no-error)
  (defvar gnus-summary-mode-map)
  (defvar bbdb-message-all-addresses)

  (bbdb-initialize 'gnus 'message)
  (bbdb-mua-auto-update-init 'gnus 'message))

;; Keyboard macros
;;;;;;;;;;;;;;;;;;

(fset 'perso-show-installed-packages
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217843 111 dead-circumflex 32 73 return] 0 "%d")) arg)))


;; Local configuration
;;;;;;;;;;;;;;;;;;;;;;;

(when (file-exists-p personalconf--local-init-file)
  (load-file personalconf--local-init-file))


;;; init.el ends here
