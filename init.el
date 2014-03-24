;;; init.el --- Where all the magic begins
;;
;; This file loads Org-mode and then loads the rest of our Emacs initialization from Emacs lisp
;; embedded in literate Org-mode files.

;; Load up Org Mode and (now included) Org Babel for elisp embedded in Org Mode files

;;; Commentary:
;;
;;; Code:

(defvar dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))

(require 'org)
;; load up all literate org-mode files in this directory
(mapc #'org-babel-load-file (directory-files dotfiles-dir t "\\.org$"))

(provide 'init)

;;; init.el ends here
