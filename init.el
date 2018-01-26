;;; init.el --- Main entry point of Emacs configuration

;;; Commentary:
;;
;;; Code:

; Done later
; (package-initialize)

(defconst my-init-dir (file-name-directory (or load-file-name (buffer-file-name))))
(defconst my-site-lisp (concat my-init-dir "site-lisp/"))

(add-to-list 'load-path my-site-lisp)

(setq custom-file (concat my-init-dir "custom-file.el"))
(load custom-file 'no-error)

(defun my-load-init-file (file)
  "Load one initialization file.

FILE is the name of the file without extension and directory."
  (load (concat my-init-dir "init/" file ".el")))

(mapc #'my-load-init-file
      '("core"
	"elpa"
	"exec-path-from-shell"
	"prompt"
	"theme"
	"neotree"
	"ivy"
	"shell"
	"ag"
	"editing"
	"git"
	"project"
	"auto-completion"
	"code-checking"
	"lsp"
	"rocktl"
	"emacs-lisp"
	"web"
	"javascript"
	"flow"
	"prettier"
	"cucumber"
	"markdown"
	"yaml"
	"android"
	"reason"
	))

;;; init.el ends here
