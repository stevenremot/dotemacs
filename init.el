;;; init.el --- Main entry point of Emacs configuration

;;; Commentary:
;;
;;; Code:

; Done later
; (package-initialize)

(defconst my-init-dir (file-name-directory (or load-file-name (buffer-file-name))))

(setq custom-file (concat my-init-dir "custom-file.el"))
(load custom-file 'no-error)

(defun my-load-init-file (file)
  "Load one initialization file.

FILE is the name of the file without extension and directory."
  (load (concat my-init-dir "init/" file ".el")))

(mapc #'my-load-init-file
      '("core"
	"theme"
	"ivy"
	"ag"
	"editing"
	"git"
	"project"
	"auto-completion"
	"code-checking"
	"lsp"
	"emacs-lisp"
	"javascript"
	"flow"
	))

;;; init.el ends here

