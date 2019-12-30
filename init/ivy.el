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
  :init (init/setup-ivy))

(use-package counsel
  :after (ivy)
  :init (counsel-mode 1))

(use-package swiper
  :after (ivy)
  :bind (("C-s" . swiper)))

(use-package ivy-posframe
  :after (ivy)
  :init (ivy-posframe-mode 1)
  :custom ((ivy-posframe-display-functions-alist
	    '((t . ivy-posframe-display-at-frame-center)))))

;;; ivy.el ends here
