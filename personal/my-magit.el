;; Using e on a diff just does diff (working directory vs HEAD) and not stage
;; with a 3-way merge view (HEAD, index, working directory):
(setq magit-ediff-dwim-show-on-hunks t)
