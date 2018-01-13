;;; ivy.el --- Setup ivy

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(defun init/setup-ivy ()
  "Setup the ivy package."
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))



(use-package ivy
  :ensure
  :init (init/setup-ivy))

(use-package counsel
  :ensure
  :after (ivy)
  :init (counsel-mode 1))

(use-package swiper
  :ensure
  :after (ivy)
  :bind (("C-s" . swiper)))

;;; ivy.el ends here
