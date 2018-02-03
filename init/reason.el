;;; reason.el --- Reason support setup

;;; Commentary:
;;

;;; Code:

(defun init/init-reason-mode ()
  "Init reason dependencies."
  (add-hook 'reason-mode-hook #'init/setup-reason-buffer)

  (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
    (when (and opam-share (file-directory-p opam-share))
      ;; Register Merlin
      (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
      (autoload 'merlin-mode "merlin" nil t nil)
      (add-hook 'reason-mode-hook 'merlin-mode t)
      ;; Use opam switch to lookup ocamlmerlin binary
      (setq merlin-command 'opam))))

(defun init/setup-reason-buffer ()
  "Setup a buffer for sorking wioth reason."
  (add-hook 'before-save-hook #'refmt-before-save))



(use-package reason-mode
  :init (init/init-reason-mode)
  :quelpa (reason-mode :repo "reasonml-editor/reason-mode" :fetcher github :stable t))

;;; reason.el ends here
