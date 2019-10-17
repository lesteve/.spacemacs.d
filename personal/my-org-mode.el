;; org-mode for getting organised + taking notes

;; Only configure org after org has been loaded. See
;; http://spacemacs.org/layers/+emacs/org/README.html#important-note for more
;; details.
(with-eval-after-load 'org

  (require 'org-install)
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

  ;; appt functionalities (appointment reminder)
  (run-at-time nil 900 'org-agenda-to-appt)
  (appt-activate t)

  ;; 10 and 0 minutes remaining warnings
  (setq appt-message-warning-time 10)
  (setq appt-display-interval 10)

  ;; Update appt each time agenda opened.
  ;; (add-hook 'org-finalize-agenda-hook 'my-org-agenda-to-appt)

  ;; This is for custom appt notifications
  (setq appt-display-format 'window)
  (setq appt-disp-window-function (function my-appt-disp-window))


  (defun my-appt-disp-window (min-to-app new-time msg)
    (save-window-excursion (shell-command (concat
                                           "/usr/bin/kdialog --title='Appointment' --passivepopup '<p style=\"background: yellow; color: purple\">"
                                           msg " (" min-to-app " minutes left)<br></p>""' 30") nil nil)))

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
        (list "~/org/todo.org" "~/org/zimbra-calendar.org" "~/org/zimbra-private.org"))

  ;; remember functionalities (to quickly type notes when they pop out in your mind)
  (setq org-default-notes-file "~/org/notes.org")
  ;;(org-remember-insinuate)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "SOMEDAY(s)" "STARTED(S!/!)" "WAITING(w/!)" "|" "DONE(d)" "DEFERRED(D)" "CANCELLED(c)")))

  (defun my-focus-emacs-window ()
    (call-process "wmctrl" nil nil nil "-x" "-a" "emacs"))

  (setq org-capture-templates
        '(("c" "Refile later" entry (file+headline "~/org/refile.org" "Captured")
           "* %?\nEntered on %T\n%i\n")
          ("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")
          ("w" "Webpage" entry (file+headline "~/org/refile.org" "Webpages")
           "* %:description\nEntered on %U\nSource: %:link\n\n%:initial%? %(progn (my-focus-emacs-window) \"\")")))

  ;; to make a TODO item dependent of its subtrees items
  (setq org-enforce-todo-dependencies t)

  ;; keep the time when a task has been completed
  (setq org-log-done 'time)
  ;; to put TODO states changes in a drawer
  (setq org-log-into-drawer t)

  ;; refile-targets
  (setq org-refile-targets (quote (("todo.org" :maxlevel . 1)
                                   ("someday.org" :level . 2)
                                   ("journal.org" :maxlevel . 2)
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

  ;; ;; org-mode-hook
  (require 'yasnippet)

  (add-hook 'org-mode-hook
            (lambda ()
              (auto-fill-mode t)
              (make-variable-buffer-local 'yas/trigger-key)
              (setq-local yas/trigger-key [tab])
              (define-key yas/keymap [tab] 'yas/next-field-group)
              ))

  ;; Encryption of tags crypt (e.g. for saving passwords and stuff)
  (require 'org-crypt)
  ;; Encrypt all entries before saving
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  ;; Use symmetric encryption
  (setq org-crypt-key nil)

  ;; To save the clock history across Emacs sessions
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  ;; To archive into a datetree into journal.org
  (setq org-archive-location "~/org/journal.org::datetree/")

  ;; org export for reveal.js presentations
  (require 'org-re-reveal)

  ;; org-protocol for capturing from firefox
  (require 'org-protocol)

  ;; Make windmove work in org-mode:
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)

  ;;; calendar synchronization
  (require 'org-caldav)

  (setq org-caldav-url "https://zimbra.inria.fr/dav/lesteve")
  (setq org-caldav-calendars
        '((:calendar-id "Calendar"
                        :inbox "~/org/zimbra-calendar.org")
          (:calendar-id "private"
                        :inbox "~/org/zimbra-private.org")))

  (setq org-icalendar-timezone "Europe/Paris")
  ;; This makes sure to-do items as a category can show up on the calendar
  (setq org-icalendar-include-todo t)
  ;; This ensures all org "deadlines" show up, and show up as due dates
  (setq org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due))
  ;; This ensures "scheduled" org items show up, and show up as start times
  (setq org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo))
  ;; Ask before deleting an entry in the calendar
  (setq org-caldav-delete-calendar-entries 'ask)
  ;; When synchronizing org files between different computers, you want to have
  ;; the sync state synchronized too
  (setq org-caldav-save-directory "~/org")
  ;; same thing for org-caldav-backup-file
  (setq org-caldav-backup-file "~/org/org-caldav-backup.org")

  ;; org-caldav-sync-at-close and org-caldav-sync-with-delay are taken from
  ;; https://www.reddit.com/r/orgmode/comments/8rl8ep/making_orgcaldav_useable/e0sb5j0/
  ;; This is the sync on close function; it also prompts for save after syncing
  ;; so no late changes get lost
  (defun my-org-caldav-sync-at-close ()
    (org-caldav-sync)
    (save-some-buffers))

  ;; This is the delayed sync function; it waits until emacs has been idle for
  ;; "secs" seconds before syncing.  The delay is important because the caldav-sync
  ;; can take five or ten seconds, which would be painful if it did that right at save.
  ;; This way it just waits until you've been idle for a while to avoid disturbing
  ;; the user.
  (defvar my-org-caldav-sync-timer nil
    "Timer that `org-caldav-push-timer' used to reschedule itself, or nil.")
  (defun my-org-caldav-sync ()
    (org-caldav-sync)
    (message (buffer-name))
    (if (eq org-caldav-sync-result nil)
        (kill-buffer "*org caldav sync result*")))

  (defun my-org-caldav-sync-with-delay (secs)
    (when my-org-caldav-sync-timer
      (cancel-timer my-org-caldav-sync-timer))
    (setq my-org-caldav-sync-timer
          (run-with-idle-timer
           (* 1 secs) nil 'my-org-caldav-sync)))

  ;; Add the delayed save hook with a five minute idle timer
  (add-hook 'after-save-hook
            (lambda ()
              (when (eq major-mode 'org-mode)
                (my-org-caldav-sync-with-delay 300))))

  ;; Add the close emacs hook
  (add-hook 'kill-emacs-hook 'my-org-caldav-sync-at-close)

  ;; Calendar display similar to Google Calendar
  (require 'calfw-org)

  ;; Content should not be indented when promoting/demoting the header
  (setq org-adapt-indentation nil)

  ;; org-babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (shell . t)
     (python . t)
     (ipython . t)
     (R . t)
     ))

  ;; Do not ask for confirmation when executing a block
  (setq org-confirm-babel-evaluate nil)

  ;; Do not indent by 2 spaces inside src blocks
  (setq org-src-preserve-indentation t)

  ;; ;; #+LaTeX_CLASS: beamer in org files
  ;; (unless (boundp 'org-export-latex-classes)
  ;;   (setq org-export-latex-classes nil))
  ;; (add-to-list 'org-export-latex-classes
  ;;   ;; beamer class, for presentations
  ;;   '("beamer"
  ;;      "\\documentclass[11pt]{beamer}\n
  ;;       \\mode<{{{beamermode}}}>\n
  ;;       \\usetheme{{{{beamertheme}}}}\n
  ;;       \\usecolortheme{{{{beamercolortheme}}}}\n
  ;;       \\beamertemplateballitem\n
  ;;       \\setbeameroption{show notes}
  ;;       \\usepackage[utf8]{inputenc}\n
  ;;       \\usepackage[T1]{fontenc}\n
  ;;       \\usepackage{hyperref}\n
  ;;       \\usepackage{color}
  ;;       \\usepackage{listings}
  ;;       \\lstset{numbers=none,language=[ISO]C++,tabsize=4,
  ;;   frame=single,
  ;;   basicstyle=\\small,
  ;;   showspaces=false,showstringspaces=false,
  ;;   showtabs=false,
  ;;   keywordstyle=\\color{blue}\\bfseries,
  ;;   commentstyle=\\color{red},
  ;;   }\n
  ;;       \\usepackage{verbatim}\n
  ;;       \\institute{{{{beamerinstitute}}}}\n
  ;;        \\subject{{{{beamersubject}}}}\n"

  ;;      ("\\section{%s}" . "\\section*{%s}")

  ;;      ("\\begin{frame}[fragile]\\frametitle{%s}"
  ;;        "\\end{frame}"
  ;;        "\\begin{frame}[fragile]\\frametitle{%s}"
  ;;        "\\end{frame}")))

  ;;   ;; letter class, for formal letters

  ;;   (add-to-list 'org-export-latex-classes

  ;;   '("letter"
  ;;      "\\documentclass[11pt]{letter}\n
  ;;       \\usepackage[utf8]{inputenc}\n
  ;;       \\usepackage[T1]{fontenc}\n
  ;;       \\usepackage{color}"

  ;;      ("\\section{%s}" . "\\section*{%s}")
  ;;      ("\\subsection{%s}" . "\\subsection*{%s}")
  ;;      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  ;;      ("\\paragraph{%s}" . "\\paragraph*{%s}")
  ;;      ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
)
