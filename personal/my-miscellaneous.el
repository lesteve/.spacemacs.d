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
;; don't want. For some reason I could not get it to work with
;; evil-motion-state-map and had to use evil-normal-state-map ...
(define-key evil-normal-state-map [down-mouse-1] 'ignore)
(define-key evil-normal-state-map [mouse-1] 'ignore)

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
