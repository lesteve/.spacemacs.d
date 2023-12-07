;; Using e on a diff just does diff (working directory vs HEAD) and not stage
;; with a 3-way merge view (HEAD, index, working directory):
(setq magit-ediff-dwim-show-on-hunks t)

;; This has the token for magit/forge
(setq auth-sources '("~/.authinfo.gpg"))

(setq  forge-topic-list-limit '(100 . 0))

(setq  forge-topic-list-limit '(100 . -10))

(with-eval-after-load 'magit
  (require 'forge))
