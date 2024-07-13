(use-package mu4e
  :defer t
  :config
  ;;; Set up some common mu4e variables
  (setq mu4e-trash-folder "/Trash"
        mu4e-refile-folder "/Archive"
        mu4e-get-mail-command "mbsync -a"
        mu4e-update-interval nil
        mu4e-compose-signature-auto-include nil
        mu4e-view-show-images t
        mu4e-view-show-addresses t)

  (setq my-work-query "AND (maildir:/inria/ OR maildir:/ymail/ OR maildir:/probabl/)")
  (setq my-work-folder-regex "inria\\|ymail\\|probabl")
  (setq my-personal-query "AND (maildir:/gmx/ OR maildir:/outlook/)")
  (setq my-personal-folder-regex "gmx\\|outlook")

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
            :name "probabl"
            :match-func (lambda (msg)
                          (when msg
                            (mu4e-message-contact-field-matches msg
                                                                '(:to :cc :bcc :from)
                                                                "loic@probabl.ai")))
            :vars '((user-mail-address . "loic@probabl.ai")
                    (user-full-name . "Loïc Estève")
                    (mu4e-sent-folder . "/probabl/[Gmail]/Sent Mail")
                    (mu4e-drafts-folder . "/probabl/[Gmail]/Drafts"))
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
            :enter-func (lambda () (my-context-enter-func my-personal-query my-personal-folder-regex))
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
            :enter-func (lambda () (my-context-enter-func my-personal-query my-personal-folder-regex))
            )
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
          ("/ymail/Inbox" . ?y)
          ("/probabl/Inbox" . ?p)
          ("/gmx/Inbox" . ?g)
          ("/outlook/Inbox" . ?t)
          ))

  ;;; Bookmarks
  (defun my-add-query-to-bookmark (query bookmark)
    (if (null query)
        bookmark
      (cons (concat (car bookmark)
                    (concat " " query))
            (cdr bookmark))))

  (defun my-context-enter-func (query folder-regex)
    (setq mu4e-bookmarks (mapcar (lambda (bookmark) (my-add-query-to-bookmark query bookmark)) my-default-mu4e-bookmarks))
    )

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
          ("from:loic.esteve" "Sent" ?s)
          (,(mapconcat 'identity
                      (mapcar
                        (lambda (maildir)
                          (concat "maildir:" (car maildir)))
                        mu4e-maildir-shortcuts) " OR ")
          "All inboxes" ?i)))

  ;;; Avoid "Maildir error: duplicate UID" errors with mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;;; Kill message buffer once the message is sent
  (setq message-kill-buffer-on-exit t)

  ;;; Headers height in split (headers + message) view
  (setq mu4e-headers-visible-lines 16)

  ;;; Do not ask for confirmation when pressing q
  (setq mu4e-confirm-quit nil)

  ;;; Do not add my email address when replying to all
  (setq message-dont-reply-to-names #'mu4e-personal-or-alternative-address-p)

  ;; Disable format=flowed. Giving up on devices that reflow content because it
  ;; seems to cumbersome at the time of writing. mu4e-compose-format-flowed is
  ;; nil by default but setting it explicitly.
  (setq mu4e-compose-format-flowed nil)
  ;; The next line is needed. mml-enable-flowed is t by default which causes
  ;; email to be encoded with format=flowed.
  (setq mml-enable-flowed nil)

  ;; Custom shortcuts for mu4e
  ;; -------------------------

  (defun my-mu4e-delete-thread ()
    (interactive)
    (mu4e-headers-mark-thread nil '(delete))
  )

  ;; quick update shortcut to only fetch update from all my INBOX mailboxes
  ;; (useful when getting a verification code by email that is valid for a
  ;; short time)
  (defun my-mu4e-quick-update ()
    (interactive)
    (let (
          (mu4e-get-mail-command "mbsync inria:INBOX outlook:INBOX gmx:INBOX ymail:INBOX ymail:Bulk probabl:INBOX"))
      (mu4e-update-mail-and-index nil)
      )
    )

  ;; In an ideal world I would do it by setting spacemacs-evil layer variables as
  ;; shown in
  ;; https://github.com/syl20bnr/spacemacs/blob/develop/layers/+spacemacs/spacemacs-evil/README.org#note-about-evil-collection
  ;; my feeling is that the evil-collection init happens early and that the spacemacs mu4e keybindings override them
  (evil-collection-mu4e-setup)

  (evil-collection-define-key 'normal 'mu4e-main-mode-map
    "p" 'my-mu4e-quick-update
    (kbd "C-S-p") 'my-mu4e-quick-update
    )

  (evil-collection-define-key 'normal 'mu4e-headers-mode-map
    ;; override j/k to go up and down in headers when having both headers and
    ;; message view. With evil-collection j/k is the same as C-j/k which is
    ;; slightly weird. The first j in the headers switches focus to the message
    ;; and the next j moves up in the message
    "j" 'evil-next-line
    "k" 'evil-previous-line
    ;; D deletes whole thread (this is almost always what I want)
    "D" 'my-mu4e-delete-thread
    ;; d deletes message (by default d moves to Trash but I never use this)
    "d" 'mu4e-headers-mark-for-delete
    (kbd "C-S-p") 'my-mu4e-quick-update
    ;; mu 1.12 changed R to be reply only to sender (mu4e-compose-reply) and I
    ;; never got used to it. I use R for reply all
    ;; (mu4e-compose-wide-reply) as previously and P (personal) for reply to
    ;; sender (mu4e-compose-reply)
    "R" 'mu4e-compose-wide-reply
    "cr" 'mu4e-compose-wide-reply
    "P" 'mu4e-compose-reply
    "cp" 'mu4e-compose-reply
    )

  ;; add shortcut for visual-line-mode (useful for emails with long lines)
  (evil-collection-define-key 'normal 'mu4e-view-mode-map
    "zv" 'visual-line-mode
    (kbd "C-S-p") 'my-mu4e-quick-update
    ;; mu 1.12 changed R to be reply only to sender (mu4e-compose-reply) and I
    ;; never got used to it. I use R for reply all
    ;; (mu4e-compose-wide-reply) as previously and P (personal) for reply to
    ;; sender (mu4e-compose-reply)
    "R" 'mu4e-compose-wide-reply
    "cr" 'mu4e-compose-wide-reply
    "P" 'mu4e-compose-reply
    "cp" 'mu4e-compose-reply
    )

  (evil-collection-define-key 'normal 'mu4e-view-mode-map
    "zv" 'visual-line-mode
    (kbd "C-S-p") 'my-mu4e-quick-update
    )

)

;; HTML support
(use-package mu4e-contrib
  :defer t
  :after mu4e
  :init
  (setq mu4e-html2text-command 'mu4e-shr2text)
  (setq shr-color-visible-luminance-min 60)
  (setq shr-color-visible-distance-min 5)
  (setq shr-use-colors nil)
  (advice-add #'shr-colorize-region :around (defun shr-no-colourise-region (&rest ignore)))
  )

;; Sometimes, not sure when, I get the following error
;; error in process filter: Error 111: failed to open store @ /home/lesteve/.cache/mu/xapian: Unable to get write lock on /home/lesteve/.cache/mu/xapian: already locked
;; This is probably similar to
;; https://github.com/djcb/mu/issues/8#issuecomment-4672468, but having a
;; work-around inside emacs makes sense
(defun my-mu4e-restart ()
  (interactive)
  (progn
    ;; Helps keeping a clean layout when restarting and having both headers and
    ;; view window
    (delete-other-windows)
    ;; important to wait here otherwise you actually create the same error you
    ;; were trying to solve
    (shell-command "killall --signal INT --wait mu")
    (mu4e-quit)
    ;; Sleeping a bit seems to help, don't ask me why ...
    (sleep-for 0.2)
    (mu4e)
    )
  )

(spacemacs/set-leader-keys "aer" 'my-mu4e-restart)

