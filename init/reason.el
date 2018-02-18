;;; reason.el --- Reason support setup

;;; Commentary:
;;

;;; Code:

(defun init/setup-reason-buffer ()
  "Setup a buffer for sorking wioth reason."
  (add-hook 'before-save-hook #'refmt-before-save))



(use-package reason-mode
  :ensure
  :hook ((reason-mode . init/setup-reason-buffer)
	 (reason-mode . merlin-mode))
  :mode "\\.re\\'"
  )

;;; reason.el ends here
