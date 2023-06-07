;;; comint-extras.el --- Extra functions for working with comint

;;; Commentary:
;;

(require 'comint)

;;; Code:

(defvar ce--global-process nil
  "Comint process that can be accessed globally.")

(defun ce-set-process-globally ()
  "Make the current comint process available globally."
  (interactive)
  (setq ce--global-process (get-buffer-process (current-buffer))))

(defun ce--get-comint-global-process ()
  "Return the current comint global process.

Throw an error if there is no process, or if it is finished."
  (when (or (null ce--global-process) (not (string= "run" (process-status ce--global-process))))
    (ce-node-repl))

  ce--global-process)

(defun ce-send-buffer ()
  "Send the whole buffer source to the current comint process."
  (interactive)
  (ce-send-region (point-min) (point-max)))

(defun ce-send-region (start end)
  (interactive "r")
  (let ((proc (ce--get-comint-global-process)))
    (comint-send-string proc ".editor\n")
    (comint-send-region proc start end)
    (comint-send-string proc "\n\x04")
    (display-buffer (process-buffer proc))))

(defun ce-send-sentence ()
  (interactive)
  (if-let ((bounds (bounds-of-thing-at-point 'sentence)))
      (ce-send-region (car bounds) (cdr bounds))
    (message "No sentence found")))

(defun ce-node-repl ()
  "Start a node REPL."
  (interactive)
  (make-comint "node-repl" "node")
  (with-current-buffer "*node-repl*"
    (ce-set-process-globally)))

(defvar ce--minor-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c C-b") #'ce-send-buffer)
    (define-key keymap (kbd "C-c C-r") #'ce-send-region)
    (define-key keymap (kbd "C-M-x") #'ce-send-sentence)
    keymap))

(define-minor-mode ce--minor-mode
  "Minor mode to send inputs to the current comint process."
   :keymap ce--minor-mode-map)

(provide 'comint-extras)

;;; comint-extras.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ce-" . "comint-extras-"))
;; End:
