;;; cedet.el --- CEDET initialization module

;;; Commentary:
;;
;;; Code:
(defvar my-site-lisp)
(load-file (concat my-site-lisp "cedet/cedet-devel-load.el"))
(load-file (concat my-site-lisp "cedet/contrib/cedet-contrib-load.el"))

(global-ede-mode 1)

(semantic-load-enable-gaudy-code-helpers)
(semantic-load-enable-all-exuberent-ctags-support)

(global-set-key (kbd "C-c -") 'senator-fold-tag-toggle)

(defun my-read-protection (prompt &optional initial-input history default-value inherit-input-method)
  "Read a protection from the minibuffer.

PROMPT is the prompt message
INITIAL-INPUT is the pre-filled input
HISTORY is the input history
DEFAULT-VALUE is value for no input.
INHERIT-INPUT-METHOD : see `read-string'"
  (completing-read prompt
                   '("public" "private" "protected")
                   nil
                   t
                   initial-input
                   history
                   default-value
                   inherit-input-method))

(provide 'init/cedet)

;;; cedet.el ends here
