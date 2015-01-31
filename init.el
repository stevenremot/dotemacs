(require 'org)
(org-babel-load-file (concat
                      (file-name-directory (or buffer-file-name load-file-name))
                      "load-modules.org") t)
