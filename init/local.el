;;; local.el --- Local init module

;;; Commentary:
;;
;;; Code:

(add-to-list 'load-path "~/Documents/Projets/roguel-ike")
(add-to-list 'load-path "~/Documents/Programmation/Emacs Lisp/Wisent test")
(add-to-list 'load-path "~/Documents/Projets/edora")
(require 'roguel-ike)
(require 'flycheck)

(flycheck-define-checker dart-dartanalyzer
  "A dart syntax checker using dartanalyzer."
  :command ("dartanalyzer" source)
  :error-patterns ((error line-start "[error] " (message) " (" (file-name) ", line " line ", col " column ")" line-end)
                   (warning line-start "[warning] " (message) " (" (file-name) ", line " line ", col " column ")" line-end)
                   (info line-start "[hint] " (message) " (" (file-name) ", line " line ", col " column ")" line-end))
  :modes dart-mode)

(add-to-list 'flycheck-checkers 'dart-dartanalyzer)

(ede-cpp-root-project "INFI-Rendu"
                      :directory "/home/steven/Documents/Programmation/3D/infsi350_tp_rendu/gmini/"
                      :file "/home/steven/Documents/Programmation/3D/infsi350_tp_rendu/gmini/Makefile"
                      :system-include-path '("/usr/include/c++/4.8/")
                      :spp-table '(("GPU_SHADERS" . "")))

(provide 'init/local)

;;; local.el ends here
