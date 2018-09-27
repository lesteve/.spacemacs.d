;;; Add locally compiled mu4e to load-path (Ubuntu 16.04 has mu4e/maildir-utils 0.9.12)
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

;;; Set up some common mu4e variables
(setq mu4e-maildir "~/.mail"
      mu4e-trash-folder "/Trash"
      mu4e-refile-folder "/Archive"
      mu4e-get-mail-command "mbsync -a"
      mu4e-update-interval nil
      mu4e-compose-signature-auto-include nil
      mu4e-view-show-images t
      mu4e-view-show-addresses t)

(setq mu4e-account-alist
      '(
        ("inria"
         (mu4e-sent-folder "/inria/Sent")
         (mu4e-drafts-folder "/inria/Drafts")
         (user-mail-address "loic.esteve@inria.fr")
         (user-full-name "Loïc Estève")
         )
        ("ymail"
         (mu4e-sent-folder "/ymail/Sent")
         (mu4e-drafts-folder "/ymail/Drafts")
         (user-mail-address "loic.esteve@ymail.com")
         (user-full-name "Loïc Estève")
         )
        ("gmx"
         (mu4e-sent-folder "/gmx/Sent")
         (mu4e-drafts-folder "/gmx/Drafts")
         (user-mail-address "loic.esteve@gmx.com")
         (user-full-name "Loïc Estève")
         )
        ))

(mu4e/mail-account-reset)

;; sending mail
(setq message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "/usr/bin/msmtp"
      user-full-name "Loïc Estève")
;; tell msmtp to choose the SMTP server according to the from field in the outgoing email
(setq message-sendmail-extra-arguments '("--read-envelope-from"))
(setq message-sendmail-f-is-evil 't)

;;; Mail directory shortcuts
(setq mu4e-maildir-shortcuts
      '(("/inria/Inbox" . ?i)
        ("/ymail/Inbox" . ?y)
        ("/gmx/Inbox" . ?g)
        ))


;;; HTML support
(require 'mu4e-contrib)
(setq mu4e-html2text-command 'mu4e-shr2text)
(setq shr-color-visible-luminance-min 60)
(setq shr-color-visible-distance-min 5)
(setq shr-use-colors nil)
(advice-add #'shr-colorize-region :around (defun shr-no-colourise-region (&rest ignore)))

;;; Bookmarks
(setq mu4e-bookmarks
      `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
        ("date:today..now" "Today's messages" ?t)
        ("date:7d..now" "Last 7 days" ?w)
        ("mime:image/*" "Messages with images" ?p)
        (,(mapconcat 'identity
                     (mapcar
                      (lambda (maildir)
                        (concat "maildir:" (car maildir)))
                      mu4e-maildir-shortcuts) " OR ")
         "All inboxes" ?i)))
