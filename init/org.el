;;; org.el --- Org mode configuration

;;; Commentary:
;;

;;; Code:

(use-package org-agenda
  :bind (("C-c o a" . org-agenda-list)))

(use-package org-clock
  :bind (("C-c o j" . org-clock-jump-to-current-clock)))

;;; org.el ends here
