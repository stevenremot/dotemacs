;;; shell.el --- Shell modes setup

;;; Commentary:
;; 
;;; Code:

(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;; shell.el ends here
