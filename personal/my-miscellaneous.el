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
