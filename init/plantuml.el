;;; plantuml.el --- PlantUML init module

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(defvar my-confdir)
(use-package plantuml-mode
  :config (setq plantuml-jar-path (concat my-confdir "programs/plantuml.jar")))

(provide 'init/plantuml)

;;; plantuml.el ends here
