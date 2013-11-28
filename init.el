;; Base configuration
;;;;;;;;;;;;;;;;;;;;;
(setq personalconf--basedir (file-name-directory load-file-name))
(setq personalconf--custom-lisp-dir (concat personalconf--basedir "/site-lisp"))
(setq personalconf--custom-conf (concat personalconf--basedir "/custom-configuration.el"))

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

;; Semantic mode
;;;;;;;;;;;;;;;;
(semantic-mode 1)
(add-to-list 'semantic-new-buffer-setup-functions '(js3-mode . wisent-javascript-setup-parser))

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

;; Flymake
;;;;;;;;;;;

;; Uncomment this to automatically enable flymake on supported files
;; (add-hook 'find-file-hook 'flymake-find-file-hook)
(global-set-key (kbd "C-M-d") 'flymake-display-err-menu-for-current-line)

;; Auto complete
;;;;;;;;;;;;;;;;

(require 'auto-complete)
(global-auto-complete-mode t)

;; Achievements
;;;;;;;;;;;;;;;

;; (require 'achievements)
;; (require 'basic-achievements)

;; tss
;;;;;;

(add-to-list 'load-path (concat personalconf--custom-lisp-dir "/emacs-tss"))
(require 'typescript)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(require 'tss)

;; Key bindinges
(setq tss-popup-help-key "C-:")
(setq tss-jump-to-definition-key "C->")

(add-to-list 'tss-ac-trigger-command-keys "=")
(tss-config-default)

;; Multi web mode
;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (concat personalconf--custom-lisp-dir "/multi-web-mode"))
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags 
  '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
    (js3-mode  "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
    (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("ctp" "phtml"))
(multi-web-global-mode 1)
