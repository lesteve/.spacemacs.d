;; So I can use accents when I feel like writing some French
(require 'iso-transl)

;; Default fill column, used e.g. by M-q
(setq-default fill-column 79)

;; Wrap lines to fill-column in visual-mode (useful e.g. when reading emails in
;; mu4e). For some reason just using visual-fill-column-mode does not work
;; (i.e. visual-fill-column never gets turned off)
(require 'visual-fill-column)
(defun my-visual-fill-column-mode ()
  (if visual-fill-column-mode
      (progn (setq visual-fill-column-width nil)
             (visual-fill-column-mode 0))
    (progn (setq visual-fill-column-width 120)
           (visual-fill-column-mode 1))))

(setq visual-line-mode-hook #'my-visual-fill-column-mode)

;; Disable mouse globally
(require 'disable-mouse)
(global-disable-mouse-mode)
;; Need this to disable mouse in evil-mode. It just happens too often that I
;; touch the touchpad while typing, which causes to insert text somewhere I
;; don't want. Taken from https://github.com/purcell/disable-mouse/issues/1#issuecomment-449902949
(mapc #'disable-mouse-in-keymap
      (list evil-motion-state-map
            evil-normal-state-map
            evil-visual-state-map
            evil-insert-state-map))
;; Not sure why but I need this on top ...
(evil-define-key 'normal 'custom-mode-map [down-mouse-1] 'disable-mouse--handle)

;; (define-key evil-motion-state-map [down-mouse-1] 'silence)
;; (define-key evil-motion-state-map [mouse-1] 'silence)
;; (define-key evil-normal-state-map [down-mouse-1] 'ignore)
;; (define-key evil-normal-state-map [mouse-1] 'ignore)

;; Prefer splitting horizontally i.e. windows side-by-side rather than on top
;; of each other. This is useful for magit status or org-agenda.
(setq split-height-threshold nil)
(setq split-width-threshold 20)

;; Keep C-i different than TAB when using emacs GUI. This is useful to use C-i
;; for evil-jump-forward i.e. move forward in the Jump List
(setq dotspacemacs-distinguish-gui-tab t)

;; Custom shortcut to use org-rifle on my org files with latest modified files
;; first. This is more useful than the default SPC a o / which only use opened
;; .org files.
(defun my-helm-org-rifle ()
  (interactive)
  (helm-org-rifle-files
   (mapcar #'car
           (sort
            ;; the pattern excludes lock files which starts with .#
            ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Locks.html#File-Locks
            (directory-files-and-attributes "~/org" t "^[^.][^#].*\.org")
            #'(lambda (x y) (not (time-less-p (nth 6 x) (nth 6 y))))
            )
           )
   )
  )
(spacemacs/set-leader-keys "ao/" 'my-helm-org-rifle)

;; Set yasnippets folder
(setq auto-completion-private-snippets-directory "~/.spacemacs.d/snippets")

;; Set prompt regexp for d d to work in vterm (otherwise it tries to delete the
;; prompt and errors with Buffer is read-only)
(setq term-prompt-regexp "^.+❯ *")

;; Set Monday as first day of the week in the calendar
(setq calendar-week-start-day 1)

;; Calendar holidays
(setq holiday-french-holidays
      `((holiday-fixed 1 1 "New Year's day")
        (holiday-fixed 5 1 "Labour day")
        (holiday-fixed 5 8 "Victory in Europe day")
        (holiday-fixed 7 14 "Bastille day")
        (holiday-fixed 8 15 "Assumption")
        (holiday-fixed 11 1 "All Saint's day")
        (holiday-fixed 11 11 "Armistice day")
        (holiday-fixed 12 25 "Christmas")
                                        ; variable holidays
        (holiday-easter-etc 0 "Easter")
        (holiday-easter-etc 1 "Easter Monday")
        (holiday-easter-etc 39 "Ascension day")
        (holiday-easter-etc 49 "Pentecost")
        (holiday-easter-etc 50 "Whit Monday")
        ))

(setq calendar-holidays holiday-french-holidays)

;; Disable saving undo history. I don't really use this feature and it takes
;; some time when saving file (e.g. zimbra synced calendar files)
(with-eval-after-load 'undo-tree
  (setq undo-tree-auto-save-history nil))

;; Only consider stdout in emacs-jupyter jupyter-command to work-around issue
;; with Python 3.11, see https://github.com/nnicandro/emacs-jupyter/pull/423.
;; This uses (require jupyter) because I was not able to make it work with
;; with-eval-after-load
(require 'jupyter)
(defun jupyter-command (&rest args)
  "Redefine jupyter-command from emacs-jupyter jupyter-env.el to ignore stderr.

The only difference is to use '(t nil) instead of t to discard stderr.
"
  (with-temp-buffer
    (when (zerop (apply #'process-file "jupyter" nil '(t nil) nil args))
      (string-trim-right (buffer-string)))))

;; Useful for khalel that has buffer-read-only at the beginning of the imported
;; org file. This is here rather than in my-org-mode.el because my-org-mode.el
;; is only loaded after org is loaded.
(push (cons 'buffer-read-only '1) safe-local-variable-values)

;; Easy switch to todo.org headline. I put it here so that it is accessible
;; even if I have not opened any org file yet
(defun my-todo-switch ()
  (interactive)
  (let*
      ((todo-basename "todo.org")
       (todo-filename (concat org-directory "/" todo-basename))
       )
    (find-file todo-filename)
    ))

(defun my-todo-headings-switch ()
  (interactive)
  (my-todo-switch)
  (spacemacs/helm-jump-in-buffer)
  )
(spacemacs/set-leader-keys "bt" 'my-todo-switch)
(spacemacs/set-leader-keys "bj" 'my-todo-headings-switch)

;; Colorize logs with ANSI escape characters
(defun my-ansi-color-buffer ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max))
  )
