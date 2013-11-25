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

(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" .
               "http://melpa.milkbox.net/packages/"))

(package-initialize)

;; Flymake
;;;;;;;;;;;

;; (add-hook 'find-file-hook 'flymake-find-file-hook)
(global-set-key (kbd "C-M-d") 'flymake-display-err-menu-for-current-line)

;; Auto complete
;;;;;;;;;;;;;;;;

(require 'auto-complete)
;; dirty fix for having AC everywhere
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
                         (auto-complete-mode 1))
                       ))
(real-global-auto-complete-mode t)

;; Achievements
;;;;;;;;;;;;;;;

(require 'achievements)
(require 'basic-achievements)

;; tss
;;;;;;

(add-to-list 'load-path (concat custom-lisp-dir "/emacs-tss"))
(require 'typescript)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(require 'tss)

;; Key bindinges
(setq tss-popup-help-key "C-:")
(setq tss-jump-to-definition-key "C->")

(add-to-list 'tss-enable-modes 'hoge-mode)
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
 '(ecb-options-version "2.40")
 '(electric-indent-mode t)
 '(electric-pair-mode t)
 '(js3-indent-level 4))
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
