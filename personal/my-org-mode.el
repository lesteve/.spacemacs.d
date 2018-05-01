;; org-mode for getting organised + taking notes

;; set the org directory to my private copy instead of the one inside
;; the emacs CVS-snapshot
;; (setq load-path (cons "~/.emacs.d/org-mode/lisp" load-path))

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

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
      (list "~/org/todo.org"))

;; remember functionalities (to quickly type notes when they pop out in your mind)
(setq org-default-notes-file "~/org/notes.org")
;;(org-remember-insinuate)
(define-key global-map (kbd "C-c c") 'org-capture)

(setq org-todo-keywords
      '((sequence "TODO(t)" "SOMEDAY(s)" "STARTED(S!/!)" "WAITING(w/!)" "|" "DONE(d)" "DEFERRED(D@)" "CANCELLED(c@)")))

(setq org-capture-templates
      '(("c" "Refile later" entry (file+headline "~/org/refile.org" "Captured")
         "* %?\nEntered on %T\n%i\n")
        ("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("w" "Webpage" entry (file+headline "~/org/refile.org" "Webpages")
         "* %c\nEntered on %U\nSource: %:link\n%i\n%?")))

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

;; Need that in order for heml completion to work. From
;; http://emacs.stackexchange.com/questions/14535/how-can-i-use-helm-with-org-refile
(setq org-outline-path-complete-in-steps nil)

; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))

; Targets complete in steps so we start with filename, TAB shows the
; next level of targets etc
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

;; To default to a side by side display for agenda views
(defadvice org-agenda (around tweak-splitting-behavior activate)
  "Tweak the sensible window splitting behavior for org-todo."
  (let ((split-height-threshold nil)
        (split-width-threshold 20)
        )
    ad-do-it))

;; Encryption of tags crypt (e.g. for saving passwords and stuff)
(require 'org-crypt)
; Encrypt all entries before saving
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
; GPG key to use for encryption
(setq org-crypt-key "F0B66B40")

;; To save the clock history across Emacs sessions
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; To archive into a datetree into journal.org
(setq org-archive-location "~/org/journal.org::datetree/")

;; org export for reveal.js presentations
(require 'ox-reveal)

;; org-protocol for capturing from firefox
(require 'org-protocol)

;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

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

