;;; clojure.el --- Clojure init module

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(use-package cider
  :ensure cider)
(use-package clojure-mode
  :ensure clojure-mode)
(use-package clojurescript-mode
  :ensure clojurescript-mode)
(use-package cljsbuild-mode
  :ensure cljsbuild-mode)

(provide 'init/clojure)
;;; clojure.el ends here
