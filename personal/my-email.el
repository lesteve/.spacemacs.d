;;; Set up some common mu4e variables
(setq mu4e-maildir "~/.mail"
      mu4e-trash-folder "/Trash"
      mu4e-refile-folder "/Archive"
      mu4e-get-mail-command "mbsync -a"
      mu4e-update-interval nil
      mu4e-compose-signature-auto-include nil
      mu4e-view-show-images t
      mu4e-view-show-addresses t)

;; Contexts for my different email accounts
(setq mu4e-contexts
      `( ,(make-mu4e-context
           :name "inria"
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches msg
                                                               :to "loic.esteve@inria.fr")))
           :vars '((user-mail-address . "loic.esteve@inria.fr")
                   (user-full-name . "Loïc Estève")
                   (mu4e-sent-folder . "/inria/Sent")
                   (mu4e-drafts-folder . "/inria/Drafts")))
         ,(make-mu4e-context
           :name "ymail"
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches msg
                                                               :to "loic.esteve@ymail.com")))
           :vars '((user-mail-address . "loic.esteve@ymail.com")
                   (user-full-name . "Loïc Estève")
                   (mu4e-sent-folder . "/ymail/Sent")
                   (mu4e-drafts-folder . "/ymail/Draft")))
         ,(make-mu4e-context
           :name "gmx"
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches msg
                                                               :to "loic.esteve@gmx.com")))
           :vars '((user-mail-address . "loic.esteve@gmx.com")
                   (user-full-name . "Loïc Estève")
                   (mu4e-sent-folder . "/gmx/Sent")
                   (mu4e-drafts-folder . "/gmx/Draft")))
         ))

;; do not ask for context when starting mu4e
(setq mu4e-context-policy 'pick-first)

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
        ("/inria/SED" . ?s)
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
        ((concat
          "flag:unread AND NOT flag:trashed "
          "AND (NOT flag:list OR list:sed-pro.inria.fr OR list:sierra.inria.fr)")
         "Unread filtered messages" ?m)
        ("contact:notifications@github.com" "Github notifications" ?g)
        ("date:today..now" "Today's messages" ?t)
        ("date:7d..now" "Last 7 days" ?w)
        ("mime:image/*" "Messages with images" ?p)
        (,(mapconcat 'identity
                     (mapcar
                      (lambda (maildir)
                        (concat "maildir:" (car maildir)))
                      mu4e-maildir-shortcuts) " OR ")
         "All inboxes" ?i)))

;;; Avoid "Maildir error: duplicate UID" errors with mbsync
(setq mu4e-change-filenames-when-moving t)

;;; Show only interesting folders in main view
;; (setq mu4e-use-maildirs-extension t)

(setq mu4e-maildirs-extension-custom-list
      '("/inria/Inbox" "/inria/SED" "/inria/Lists/WillowSierra" "/inria/Lists/hpc-big-data"
        "/ymail/Inbox" "/ymail/github" "/ymail/github/dask" "/ymail/github/scikit-learn"
        "/gmx/Inbox" "/gmx/Inbox/AhOuhPuc" "/gmx/Inbox/Roc14"))

;;; Kill message buffer once the message is sent
(setq message-kill-buffer-on-exit t)

;;; Headers height in split (headers + message) view
(setq mu4e-headers-visible-lines 16)

;;; Do not ask for confirmation when pressing q
(setq mu4e-confirm-quit nil)
