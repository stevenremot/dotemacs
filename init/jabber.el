;;; jabber.el --- Jabber init module

;;; Commentary:
;;
(require 'use-package)
;;; Code:

(defconst my-jabber-machine 'jabber
  "Machine for jabber credentials in auth source.")

(defun my-jabber-setup ()
  "Setup jabber account."
  (let ((creds (auth-source-search :machine my-jabber-machine
                                   :port 'xmpp
                                   :max 1)))
    (when creds
      (let* ((account (car creds))
             (login (plist-get account :user))
             (host (plist-get account :host))
             (password (funcall (plist-get account :secret))))
        (setq jabber-account-list
              `((,login
                 (:network-server . ,host)
                 (:password . ,password))))))))

(use-package jabber
  :init (my-jabber-setup))

(provide 'init/jabber)

;;; jabber.el ends here
