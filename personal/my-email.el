;;; Set up some common mu4e variables
(setq mu4e-maildir "~/.mail"
      mu4e-trash-folder "/Trash"
      mu4e-refile-folder "/Archive"
      mu4e-get-mail-command "mbsync -a"
      mu4e-update-interval nil
      mu4e-compose-signature-auto-include nil
      mu4e-view-show-images t
      mu4e-view-show-addresses t)

(setq my-work-query "AND NOT maildir:/gmx/ AND NOT maildir:/outlook/")
(setq my-work-folder-regex "inria\\|ymail")

;; Contexts for my different email accounts
(setq mu4e-contexts
      `( ,(make-mu4e-context
           :name "inria"
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches msg
                                                               '(:to :cc :bcc :from)
                                                               "loic.esteve@inria.fr")))
           :vars '((user-mail-address . "loic.esteve@inria.fr")
                   (user-full-name . "Loïc Estève")
                   (mu4e-sent-folder . "/inria/Sent")
                   (mu4e-drafts-folder . "/inria/Drafts"))
           :enter-func (lambda () (my-context-enter-func my-work-query my-work-folder-regex))
           )
         ,(make-mu4e-context
           :name "ymail"
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches msg
                                                               '(:to :cc :bcc :from)
                                                               "loic.esteve@ymail.com")))
           :vars '((user-mail-address . "loic.esteve@ymail.com")
                   (user-full-name . "Loïc Estève")
                   (mu4e-sent-folder . "/ymail/Sent")
                   (mu4e-drafts-folder . "/ymail/Draft"))
           :enter-func (lambda () (my-context-enter-func my-work-query my-work-folder-regex))
           )
         ,(make-mu4e-context
           :name "gmx"
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches msg
                                                               '(:to :cc :bcc :from)
                                                               "loic.esteve@gmx.com")))
           :vars '((user-mail-address . "loic.esteve@gmx.com")
                   (user-full-name . "Loïc Estève")
                   (mu4e-sent-folder . "/gmx/Sent")
                   (mu4e-drafts-folder . "/gmx/Drafts"))
           :enter-func (lambda () (my-context-enter-func nil nil))
           )
         ,(make-mu4e-context
           :name "outlook"
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches msg
                                                               '(:to :cc :bcc :from)
                                                               "loic.esteve@outlook.com")))
           :vars '((user-mail-address . "loic.esteve@outlook.com")
                   (user-full-name . "Loïc Estève")
                   (mu4e-sent-folder . "/outlook/Sent")
                   (mu4e-drafts-folder . "/outlook/Drafts"))
           :enter-func (lambda () (my-context-enter-func nil nil))
           )
         ))

;; Set list of addresses from contexts
;; Note: mu4e-user-mail-address-list seems to be needed for
;; (setq mu4e-compose-dont-reply-to-self t) to have an effect
(setq mu4e-user-mail-address-list
      (delq nil
            (mapcar (lambda (context)
                      (when (mu4e-context-vars context)
                        (cdr (assq 'user-mail-address (mu4e-context-vars context)))))
                    mu4e-contexts)))

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
        ("/outlook/Inbox" . ?t)
        ))


;;; HTML support
(require 'mu4e-contrib)
(setq mu4e-html2text-command 'mu4e-shr2text)
(setq shr-color-visible-luminance-min 60)
(setq shr-color-visible-distance-min 5)
(setq shr-use-colors nil)
(advice-add #'shr-colorize-region :around (defun shr-no-colourise-region (&rest ignore)))

;;; Bookmarks
(defun my-add-query-to-bookmark (query bookmark)
  (if (null query)
      bookmark
    (cons (concat (car bookmark)
                  (concat " " query))
          (cdr bookmark))))

(defun my-filter-maildirs(folder-regex maildirs)
  (if (null folder-regex)
   my-default-mu4e-maildirs-extension-custom-list
   (seq-filter (lambda (each) (string-match-p folder-regex each))
               maildirs)))

(defun my-context-enter-func (query folder-regex)
  (setq mu4e-bookmarks (mapcar (lambda (bookmark) (my-add-query-to-bookmark query bookmark)) my-default-mu4e-bookmarks))
  (setq mu4e-maildirs-extension-custom-list
        (my-filter-maildirs folder-regex my-default-mu4e-maildirs-extension-custom-list))
  ;; Need to reset the variable used to cache the list of shown maildirs
  (setq mu4e-maildirs-extension-maildirs nil))

(setq my-default-mu4e-bookmarks
      `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
        (,(concat
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
(setq my-default-mu4e-maildirs-extension-custom-list
      '("/inria/Inbox" "/inria/SED" "/inria/Lists/WillowSierra" "/inria/Lists/hpc-big-data"
        "/ymail/Inbox" "/ymail/github" "/ymail/github/dask" "/ymail/github/scikit-learn"
        "/gmx/Inbox" "/gmx/Inbox/AhOuhPuc" "/gmx/Inbox/Roc14"
        "/outlook/Inbox"))

;;; Kill message buffer once the message is sent
(setq message-kill-buffer-on-exit t)

;;; Headers height in split (headers + message) view
(setq mu4e-headers-visible-lines 16)

;;; Do not ask for confirmation when pressing q
(setq mu4e-confirm-quit nil)

;;; Do not add my email address when replying to all
(setq mu4e-compose-dont-reply-to-self t)

;; Disable format=flowed. Giving up on devices that reflow content because it
;; seems to cumbersome at the time of writing. mu4e-compose-format-flowed is
;; nil by default but setting it explicitly.
(setq mu4e-compose-format-flowed nil)
;; The next line is needed. mml-enable-flowed is t by default which causes
;; email to be encoded with format=flowed.
(setq mml-enable-flowed nil)
