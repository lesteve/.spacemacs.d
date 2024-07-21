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

;; Do not watch Pyodide cross-build folders
(with-eval-after-load 'lsp-mode
  (setq lsp-file-watch-ignored-directories
        (append lsp-file-watch-ignored-directories '("[/\\\\]\\.pyodide-xbuildenv\\'")))
)
