;;; gtags.el --- GNU Global support initialization

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(defun my-enable-ggtags ()
  "Enable ggtags mode on different hooks."
  (dolist (mode '(fundamental-mode text-mode php-mode web-mode c++-mode python-mode prog-mode))
    (add-hook (intern (concat (symbol-name mode) "-hook"))
              (lambda ()
                (ggtags-mode 1)
                (helm-gtags-mode 1)))))

(use-package ggtags
  :ensure ggtags)

(use-package helm-gtags
  :ensure helm-gtags
  :config (progn
            (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-select)
            (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
            (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
            (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
            (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history))
  :init (my-enable-ggtags))

(provide 'init/gtags)

;;; gtags.el ends here
