;; turn on indentation highlighting
(add-hook 'python-mode-hook #'spacemacs/toggle-highlight-indentation-on)

;; Add current function to the spaceline
(add-hook 'python-mode-hook #'which-function-mode)

(defvar-local flycheck-local-checkers nil)
(defun +flycheck-checker-get(fn checker property)
  (or (alist-get property (alist-get checker flycheck-local-checkers))
      (funcall fn checker property)))
(advice-add 'flycheck-checker-get :around '+flycheck-checker-get)

;; Add python-flake8 to lsp for python-mode only for Python
;; adapted from https://github.com/weijiangan/flycheck-golangci-lint/issues/8#issuecomment-765580616
(add-hook 'python-mode-hook
          (lambda()
            (setq flycheck-local-checkers '((lsp . ((next-checkers . (python-flake8))))))))

;; Use ipython rather than plain python for the python shell
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args
      "-i --simple-prompt --matplotlib --InteractiveShell.display_page=True")

;; conda environment settings
(setenv "WORKON_HOME" (expand-file-name "~/micromamba/envs"))

;; Do not watch Pyodide cross-build folders, Pyodide venv or Meson build
;; folders
(with-eval-after-load 'lsp-mode
  (setq
   lsp-file-watch-ignored-directories
   (append lsp-file-watch-ignored-directories
           '("[/\\\\]\\.pyodide-xbuildenv\\'" "[/\\\\]\\.pyodide-venv\\'" "[/\\\\]build\\'"))))

;; 1000 by default let's raise it a bit to avoid the warnings and see what
;; happens
(setq lsp-file-watch-threshold 3000)

;; better integration with code-cells you see code + output in the Jupyter REPL
(setq jupyter-repl-echo-eval-p t)

;; better code-cells binding
(with-eval-after-load 'code-cells
  (let ((map code-cells-mode-map))
    (define-key map (kbd "C-k") 'code-cells-backward-cell)
    (define-key map [remap evil-backward-section-begin] 'code-cells-backward-cell)
    (define-key map (kbd "C-j") 'code-cells-forward-cell)
    (define-key map [remap evil-forward-section-begin] 'code-cells-forward-cell)
    (define-key map (kbd "C-<return>") 'code-cells-eval)
    (define-key map [remap evil-ret] 'my-eval-and-next-cell)
    (spacemacs/set-leader-keys-for-minor-mode 'code-cells-mode
      "," 'my-eval-and-next-cell)))

(defun my-eval-and-next-cell ()
  (interactive)
  (call-interactively 'code-cells-eval)
  (code-cells-forward-cell 1))
