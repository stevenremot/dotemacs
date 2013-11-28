;; Base configuration
;;;;;;;;;;;;;;;;;;;;;
(setq custom-lisp-dir "~/.emacs.d/site-lisp")

(add-to-list 'load-path custom-lisp-dir)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(show-paren-mode 1)

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

(add-to-list 'load-path (concat custom-lisp-dir "/emacs-tss"))
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

(add-to-list 'load-path (concat custom-lisp-dir "/multi-web-mode"))
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags 
  '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
    (js3-mode  "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
    (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("ctp" "phtml"))
(multi-web-global-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-layout-window-sizes (quote (("left8" (ecb-directories-buffer-name 0.16666666666666666 . 0.2619047619047619) (ecb-sources-buffer-name 0.16666666666666666 . 0.23809523809523808) (ecb-methods-buffer-name 0.16666666666666666 . 0.2857142857142857) (ecb-history-buffer-name 0.16666666666666666 . 0.19047619047619047)))))
 '(ecb-options-version "2.40")
 '(electric-indent-mode t)
 '(electric-pair-mode t)
 '(js3-indent-level 4)
 '(rst-adornment-faces-alist (quote ((t . font-lock-keyword-face) (nil . font-lock-keyword-face) (1 . font-lock-keyword-face) (t . font-lock-keyword-face) (3 . font-lock-keyword-face) (4 . font-lock-keyword-face) (5 . font-lock-keyword-face) (6 . font-lock-keyword-face)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "orange" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 128 :width normal :foundry "unknown" :family "Ubuntu Mono"))))
 '(bold ((t (:weight bold))))
 '(ecb-analyse-face ((t (:inherit ecb-default-highlight-face))))
 '(ecb-default-highlight-face ((t (:background "medium blue"))))
 '(ecb-directory-face ((t (:inherit ecb-default-highlight-face))))
 '(ecb-history-face ((t (:inherit ecb-default-highlight-face))))
 '(ecb-method-face ((t (:inherit ecb-default-highlight-face))))
 '(ecb-source-face ((t (:inherit ecb-default-highlight-face))))
 '(ecb-tag-header-face ((t (:background "dark green"))))
 '(error ((t (:foreground "red1" :weight thin))))
 '(region ((t (:background "indian red")))))
