;; Using e on a diff just does diff (working directory vs HEAD) and not stage
;; with a 3-way merge view (HEAD, index, working directory):
(setq magit-ediff-dwim-show-on-hunks t)

;; This has the token for magit/forge
(setq auth-sources '("~/.authinfo.gpg"))

(with-eval-after-load 'magit
  ;; Set smaller graphqlItemLimit to avoid HTTP 502 Bad gateway or HTTP 504 Gateway time-out
  ;; Following https://github.com/magit/forge/issues/69#issuecomment-1572402440
  (magit-set "forge.graphqlItemLimit" "50")
  (require 'forge)
  ;; Follow recommendation from https://github.com/magit/forge/issues/367#issuecomment-1848723122
  (setq forge-notifications-github-kludge 'pending-again)

  ;; (require 'code-review)
  ;; (setq code-review-auth-login-marker 'forge)

  ;; Limit to 50 open topics and show 10 closed topics (hidden by default)
  (setq  forge-topic-list-limit '(50 . -10))

)


