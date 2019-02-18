;; Start emacs server if not running already
(require 'server)
(unless (server-running-p) (server-start))

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
        (visual-fill-column-mode 0)
      (visual-fill-column-mode 1)))

(setq visual-line-mode-hook #'my-visual-fill-column-mode)

;; Disable auto-insertion of matching character in smartparens
(eval-after-load 'smartparens
  '(progn
     (sp-pair "(" nil :actions '(:rem insert))
     (sp-pair "[" nil :actions '(:rem insert))
     (sp-pair "'" nil :actions '(:rem insert))
     (sp-pair "\"" nil :actions '(:rem insert))
     )
  )

;; Remove window decorations around emacs X window
(toggle-frame-fullscreen)

;; Disable mouse globally
(require 'disable-mouse)
(global-disable-mouse-mode)
;; Need this to disable mouse in evil-mode. It just happens too often that I
;; touch the touchpad while typing, which causes to insert text somewhere I
;; don't want. For some reason I could not get it to work with
;; evil-motion-state-map and had to use evil-normal-state-map ...
(define-key evil-normal-state-map [down-mouse-1] 'ignore)
(define-key evil-normal-state-map [mouse-1] 'ignore)
