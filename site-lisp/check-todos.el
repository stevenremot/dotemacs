;;; check-todos.el --- Todos detection

;;; Commentary:


;;; Code:

;;;###autoload
(defun check-todos ()
  "Check if there are todos in the opened buffers."
  (interactive)
  (multi-occur-in-matching-buffers "\.php$" "@todo"))

;;; check-todos.el ends here
