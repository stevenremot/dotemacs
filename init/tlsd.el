;;; tlsd.el --- TLSD environment init module

;;; Commentary:
;;
;;; Code:
(defun tlsd-start-header ()
  "Insert the base content for a TLSD header."
  (interactive)
  (c++-mode)
  (srecode-insert "file:tlsd-header-base"))

(provide 'init/tlsd)

;;; tlsd.el ends here
