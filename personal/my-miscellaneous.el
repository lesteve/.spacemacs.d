(server-mode 1)

;; I don't want to automatically clean up trailing spaces
;; TODO
;; (setq prelude-clean-whitespace-on-save nil)

;; So I can use accents when I feel like writing some French
(require 'iso-transl)

;; Default fill column, used e.g. by M-q
(setq-default fill-column 79)

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
