;;; clojure.el --- Clojure init module

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(use-package cider)
(use-package clojure-mode)
(use-package clojurescript-mode)
(use-package cljsbuild-mode)

(provide 'init/clojure)
;;; clojure.el ends here
