;; The Helm layer owns activation; apply preferences when Helm loads.
(use-package helm
  :defer t
  :config
  ;; C-j/C-k can move between sources, without needing C-o.
  (setq helm-move-to-line-cycle-in-source nil)
  ;; Use split-width-threshold when choosing the other-window direction.
  (setq helm-window-prefer-horizontal-split 'decide)
  ;; Allow longer buffer names when using side-by-side windows.
  (setq helm-buffer-max-length 30))

(use-package helm-swoop
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all)
         :map isearch-mode-map
         ("M-i" . helm-swoop-from-isearch))
  :config
  ;; From helm-swoop to helm-multi-swoop-all
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
  ;; When doing evil-search, hand the word over to helm-swoop
  ;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

  ;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
  (define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

  ;; Move up and down like isearch
  (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
  (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

  ;; Save buffer when helm-multi-swoop-edit complete
  (setq helm-multi-swoop-edit-save t)

  ;; If this value is t, split window inside the current window
  (setq helm-swoop-split-with-multiple-windows nil)

  ;; Split direction. 'split-window-vertically or 'split-window-horizontally
  (setq helm-swoop-split-direction 'split-window-vertically)

  ;; If nil, you can slightly boost invoke speed in exchange for text color
  (setq helm-swoop-speed-or-color nil)

  ;; ;; Go to the opposite side of line from the end or beginning of line
  (setq helm-swoop-move-to-line-cycle t)

  ;; Optional face for line numbers
  ;; Face name is `helm-swoop-line-number-face`
  (setq helm-swoop-use-line-number-face t)

  ;; If you prefer fuzzy matching
  (setq helm-swoop-use-fuzzy-match t))

;; The Helm layer supplies the search commands and their autoloads.
(use-package helm-ag
  :defer t
  :config
  ;; Find matches in .* files
  (unless (member "--hidden" (split-string-and-unquote helm-ag-base-command))
    (setq helm-ag-base-command (concat helm-ag-base-command " --hidden")))

  ;; ripgrep exit status 2 is partial success, following
  ;; https://github.com/emacsorphanage/helm-ag?tab=readme-ov-file#ripgrep
  (setq helm-ag-success-exit-status '(0 2))

  ;; Taken from https://github.com/emacsorphanage/helm-ag/issues/388. Stop-gap
  ;; solution until https://github.com/syl20bnr/spacemacs/issues/16200 is fixed.
  ;; Probably the most promising long-term option is
  ;; https://github.com/emacsorphanage/helm-ag/pull/394.
  (defun helm-ag--construct-ignore-option (pattern)
    "Not documented, PATTERN."
    (concat "--glob=!" pattern)))
