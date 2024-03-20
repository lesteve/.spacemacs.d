;; org-mode for getting organised + taking notes

;; Only configure org after org has been loaded. See
;; http://spacemacs.org/layers/+emacs/org/README.html#important-note for more
;; details.
(with-eval-after-load 'org

  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

  (setq org-support-shift-select t)
  (setq org-special-ctrl-k t)
  ;; use ido completion inside org
  (setq org-completion-use-ido t)
  ;; start the agenda on the current day
  (setq org-agenda-start-on-weekday nil)
  ;; skip deadline and scheduled entries in the agenda if done
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)

  ;; diary functionalities (eg holidays show up in agenda)
  (setq org-agenda-include-diary t)

  (defun my-org-agenda-to-appt ()
    (interactive)
    ;; Make sure to refresh appt to avoid getting notifications about items
    ;; whose time has changed or which have been unscheduled
    (org-agenda-to-appt t))
  ;; appt functionalities (appointment reminder)
  (run-at-time nil 300 'my-org-agenda-to-appt)
  (appt-activate t)

  ;; 10 and 0 minutes remaining warnings
  (setq appt-message-warning-time 10)
  (setq appt-display-interval 10)

  ;; Update appt each time agenda opened.
  (add-hook 'org-finalize-agenda-hook 'my-org-agenda-to-appt)

  ;; This is for custom appt notifications
  (setq appt-display-format 'window)
  (setq appt-disp-window-function (function my-appt-disp-window))


  (defun my-appt-disp-window (min-to-app new-time msg)
    (save-window-excursion (shell-command (concat
                                           "notify-send 'Appointment' '"
                                           msg " (" min-to-app " minutes left)' -t 30000") nil nil)))

  ;; to make mailto link work properly at the moment 18/06/2010
  ;; browse-url is default and does not work for some reason maybe
  ;; because of firefox
  (defun my-mailto-program (mailto-url)
    ;;(interactive "sEnter stuff here: ")
    (save-window-excursion (shell-command (concat
                                           "thunderbird " mailto-url) nil nil )))

  (setq org-link-mailto-program '(my-mailto-program "mailto:%a?subject=%s"))
  ;; stuff to remove blank lines
  (setq org-cycle-separator-lines 0)
  (setq org-blank-before-new-entry (quote ((heading)
                                           (plain-list-item))))
  ;; set default column format
  (setq org-columns-default-format "%25ITEM %TODO %3PRIORITY %TAGS %17Effort(Estimated Effort){:} %CLOCKSUM")
  ;; define globally different entries for Effort
  (setq org-global-properties (quote (("Effort_ALL" . "0 0:10 0:30 1:00 2:00 4:00 6:00 8:00"))))



  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states) ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

  ;; files included in the agenda
  (setq org-agenda-files
        (list "~/org/todo.org" "~/org/khal-calendar.org"))

  ;; remember functionalities (to quickly type notes when they pop out in your mind)
  (setq org-default-notes-file "~/org/notes.org")
  ;;(org-remember-insinuate)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "SOMEDAY(S)" "STARTED(s!/!)" "WAITING(w/!)" "|" "DONE(d)" "DEFERRED(D)" "CANCELLED(c)")))

  (defun my-focus-emacs-window ()
    (call-process "wmctrl" nil nil nil "-x" "-a" "emacs"))

  (setq my-org-journal-file-basename (format-time-string "%Y-journal.org"))
  (setq my-org-journal-file (format "~/org/%s" my-org-journal-file-basename))

  (setq org-capture-templates
        '(("c" "Refile later" entry (file+headline "~/org/refile.org" "Captured")
           "* %?\nEntered on %T\n%i\n")
          ("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+olp+datetree my-org-journal-file)
           "* %?\nEntered on %U\n  %i\n  %a")
          ("w" "Webpage" entry (file+headline "~/org/refile.org" "Webpages")
           "* %:description\nEntered on %U\nSource: %:link\n\n%:initial%? %(progn (my-focus-emacs-window) \"\")")
          ("d" "From calfw" entry (file "~/org/zimbra-private.org")  "* %? \n%(cfw:org-capture-day)" :append t)))

  ;; to make a TODO item dependent of its subtrees items
  (setq org-enforce-todo-dependencies t)

  ;; keep the time when a task has been completed
  (setq org-log-done 'time)
  ;; to put TODO states changes in a drawer
  (setq org-log-into-drawer t)

  ;; refile-targets
  (setq org-refile-targets (quote (("todo.org" :maxlevel . 1)
                                   ("someday.org" :level . 2)
                                   (my-org-journal-file-basename :maxlevel . 2)
                                   (nil :maxlevel . 3))))

  ;; Need that in order for helm completion to work. From
  ;; http://emacs.stackexchange.com/questions/14535/how-can-i-use-helm-with-org-refile
  (setq org-outline-path-complete-in-steps nil)

  ;; Targets start with the file name - allows creating level 1 tasks
  (setq org-refile-use-outline-path (quote file))

  ;; Targets complete in steps so we start with filename, TAB shows the
  ;; next level of targets etc
  ;; commented for now (uncomment if too many completions at one point)
  ;; (setq org-outline-path-complete-in-steps t)

  (setq org-todo-keyword-faces (quote (("TODO" :foreground "red" :weight bold)
                                       ("STARTED" :foreground "cyan" :weight bold)
                                       ("DONE" :foreground "forest green" :weight bold)
                                       ("WAITING" :foreground "orange" :weight bold)
                                       ("SOMEDAY" :foreground "magenta" :weight bold)
                                       ("CANCELLED" :foreground "forest green" :weight bold)
                                       ("DEFERRED" :foreground "forest green" :weight bold)
                                       ("PROJECT" :foreground "red" :weight bold))))

  ;; Encryption of tags crypt (e.g. for saving passwords and stuff)
  (require 'org-crypt)
  ;; Encrypt all entries before saving
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  ;; Use my GPG key
  (setq org-crypt-key "loic.esteve@inria.fr")
  ;; Disable auto-save in buffer when using org-decrypt
  (setq org-crypt-disable-auto-save t)

  ;; To save the clock history across Emacs sessions
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  ;; To archive into a datetree into my journal file
  (setq org-archive-location (format "%s::datetree/" my-org-journal-file))

  ;; org export for reveal.js presentations
  (require 'org-re-reveal)

  ;; org-protocol for capturing from firefox
  (require 'org-protocol)

  ;; Make windmove work in org-mode:
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)

  ;; Content should not be indented when promoting/demoting the header
  (setq org-adapt-indentation nil)

  ;; org-babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (shell . t)
     (python . t)
     (jupyter . t)
     (R . t)
     ))

  ;; Do not ask for confirmation when executing a block
  (setq org-confirm-babel-evaluate nil)

  ;; Do not indent by 2 spaces inside src blocks
  (setq org-src-preserve-indentation t)

  ;; disable completion on newline in org-babel src blocks
  ;; https://github.com/syl20bnr/spacemacs/issues/13465#issuecomment-620684283
  (setq org-src-tab-acts-natively nil)

  ;; org-capture in full frame from capturing from anywhere
  (require 'noflet)

  (defun my-org-capture-in-new-full-frame ()
    "Create a new frame and run org-capture in full frame (i.e. no splitting).
Taken from https://stackoverflow.com/a/24643887"

    (interactive)
    (add-hook 'org-capture-after-finalize-hook 'my-org-capture-after-finalize-hook)
    (make-frame '((name . "capture")))
    (select-frame-by-name "capture")
    (delete-other-windows)
    (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
      (org-capture)))

  (defun my-org-capture-after-finalize-hook ()
    ;; Delete the frame after capture has been done
    (delete-frame)
    ;; Remove myself (i.e. the function currently executed) from the hook so that
    ;; delete-frame is only called after this capture session
    (remove-hook 'org-capture-after-finalize-hook 'my-org-capture-after-finalize-hook)
    )

  ;; org-pomodoro settings
  (use-package org-pomodoro
    :defer t
    :init
    (setq alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil))))
    (setq org-pomodoro-play-sounds nil)
    ;; Uncomment this for quicker testing
    ;; (setq
    ;;  org-pomodoro-length 1
    ;;  org-pomodoro-short-break-length 1)
    )

  (defun my-org-pomodoro-info ()
    "Return info about pomodoro state. Adapted from https://colekillian.com/posts/org-pomodoro-and-polybar/"
    (if (org-pomodoro-active-p)
        (cl-case org-pomodoro-state
          (:pomodoro
           (format "%s: %d minutes" org-clock-heading (/ (org-pomodoro-remaining-seconds) 60)))
          (:short-break
           (format "Short break: %d minutes" (/ (org-pomodoro-remaining-seconds) 60)))
          (:long-break
           (format "Long break: %d minutes" (/ (org-pomodoro-remaining-seconds) 60)))
          (:overtime
           (format "Overtime! %d minutes" (/ (org-pomodoro-remaining-seconds) 60))))
      ""))

  ;; calfw for visual calendar inside emacs
  (require 'calfw)
  (require 'calfw-org)

  (spacemacs/set-leader-keys "aoCd" 'cfw:open-org-calendar)

  ;; auto-evilification removes useful vim-like shortcuts
  (define-key cfw:calendar-mode-map (kbd "SPC") 'spacemacs-cmds)
  (define-key cfw:calendar-mode-map (kbd "TAB") 'cfw:show-details-command)
  (define-key cfw:calendar-mode-map (kbd "C-j") 'cfw:navi-next-item-command)
  (define-key cfw:calendar-mode-map (kbd "C-k") 'cfw:navi-prev-item-command)
  (define-key cfw:org-schedule-map (kbd "SPC") 'spacemacs-cmds)
  (define-key cfw:org-schedule-map (kbd "TAB") 'cfw:org-open-agenda-day)
  (define-key cfw:org-custom-map (kbd "SPC") 'spacemacs-cmds)
  (define-key cfw:org-custom-map (kbd "TAB") 'cfw:org-open-agenda-day)

  ;; org template to use for capture from calfw note the real info is in
  ;; org-template. It seems you only need to say the template key (i.e. "d" in
  ;; this case)
  (setq cfw:org-capture-template '("d" "" entry (file nil)  ""))

  ;; use flameshot for capturing screenshot from org
  (setq org-download-screenshot-method "flameshot gui --raw > %s")

  ;; khal/vdirsyncer setup
  (require 'khalel)
  (setq khalel-khal-command (expand-file-name "~/micromamba/envs/khal/bin/khal"))
  (setq khalel-vdirsyncer-command "~/micromamba/envs/khal/bin/vdirsyncer")
  (setq khalel-import-org-file (concat org-directory "/" "khal-calendar.org"))
  (setq khalel-default-calendar "private")
  (setq khalel-import-org-file-confirm-overwrite nil)
  (setq khalel-import-start-date "-30d")
  (setq khalel-import-end-date "+180d")
  (khalel-add-capture-template)
  ;; Periodically sync calendars when idle for 5 minutes
  (run-with-idle-timer 300 t 'khalel-import-events)

)
