;;; passgen.el --- Password generator

;;; Commentary:
;;

;;; Code:

(defgroup passgen
  ()
  "Password generation parameters."
  :group 'applications)

(defcustom passgen-chars
  "0123456789azertyuiopqsdfghjklmwxcvbnAZERTYUIOPQSDFGHJKLMWXCVBN!:;,?./§"
  "Valid characters for password."
  :group 'passgen
  :type 'string)

(defun passgen (pass-len)
  "Generate a password with PASS-LEN characters.

If used with a numeric prefix, PASS-LEN will have its value.  Otherwise,
will ask a value for PASS-LEN."
  (interactive "*NPassword length: ")
  (let ((pass ""))
    (dotimes (i pass-len)
      (let ((index (random (length passgen-chars))))
        (setq pass (concat pass (substring passgen-chars index (1+ index))))))
    (insert pass)))

(provide 'passgen)

;;; passgen.el ends here
