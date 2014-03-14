;;; mu4e-init.el --- Mu4e-related initialization

;;; Commentary:
;;

;;; Code:

(eval-after-load 'mu4e
  (progn
    (defvar mu4e-maildir)
    (defvar mu4e-sent-folder)
    (defvar mu4e-drafts-folder)
    (defvar mu4e-trash-folder)
    (defvar mu4e-refile-folder)
    (defvar user-mail-address)
    (defvar smtpmail-default-smtp-server)
    (defvar smtpmail-local-domain)
    (defvar smtpmail-smtp-server)
    (defvar smtpmail-stream-type)
    (defvar smtpmail-smtp-service)
    (defvar mu4e-get-mail-command)
    (defvar mu4e-update-interval)
    (defvar mu4e-compose-parent-message)

    (defvar perso-mail-roots '("/Perso/" "/Telecom/" "/Inovia/"))

    (defun perso-auto-select-mail-folder (msg dir)
      (let ((maildir (mu4e-message-field msg :maildir)))
        (catch 'selected-folder
          (dolist (root perso-mail-roots)
            (when (string-match root maildir)
              (throw 'selected-folder (concat root dir))))
          (concat "/" dir))))

    (setq mu4e-maildir "~/Maildir"
          mu4e-sent-folder (lambda (msg)
                             (perso-auto-select-mail-folder msg "Sent"))
          mu4e-drafts-folder "/Drafts"
          mu4e-trash-folder (lambda (msg)
                              (perso-auto-select-mail-folder msg "Trash"))
          mu4e-refile-folder (lambda (msg)
                               (perso-auto-select-mail-folder msg "Archive"))

          user-mail-address "steven.remot@gmail.com"
          smtpmail-default-smtp-server "smtp.gmail.com"
          smtpmail-local-domain "gmail.com"
          smtpmail-smtp-server "smtp.gmail.com"
          smtpmail-stream-type 'starttls
          smtpmail-smtp-service 25

          mu4e-get-mail-command "offlineimap"
          mu4e-update-interval 300)

    (defvar my-mu4e-account-alist
      '(("Perso"
         (user-mail-address "steven.remot@gmail.com"))
        ("Telecom"
         (user-mail-address "steven.remot@telecom-paristech.fr")
         (smtpmail-default-smtp-server "z.mines-telecom.fr")
         (smtpmail-smtp-server "z.mines-telecom.fr")
         (smtpmail-smtp-service 587))
        ("Inovia"
         (user-mail-address "steven.remot@inovia-team.com"))))

    (defun my-mu4e-set-account ()
      "Set the account for composing a message."
      (let* ((account
              (if mu4e-compose-parent-message
                  (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                    (string-match "/\\(.*?\\)/" maildir)
                    (match-string 1 maildir))
                (completing-read (format "Compose with account: (%s) "
                                         (mapconcat #'(lambda (var) (car var)) my-mu4e-account-alist "/"))
                                 (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                                 nil t nil nil (caar my-mu4e-account-alist))))
             (account-vars (cdr (assoc account my-mu4e-account-alist))))
        (if account-vars
            (mapc #'(lambda (var)
                      (set (car var) (cadr var)))
                  account-vars)
          (error "No email account found"))))

    (add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)
    ))

(provide 'mu4e-init)

;;; mu4e-init.el ends here
