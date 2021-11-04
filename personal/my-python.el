;; turn on indentation highlighting
(add-hook 'python-mode-hook #'spacemacs/toggle-highlight-indentation-on)

;; Add current function to the spaceline
(add-hook 'python-mode-hook #'which-function-mode)


;; Disable which-function-mode in Cython
;; See https://github.com/syl20bnr/spacemacs/issues/12740
(add-hook 'cython-mode-hook
          (lambda ()
            (which-function-mode -1)
            ;; the depth=10 argument is to make sure this hook is called last
            ) 10)

;; Use ipython rather than plain python for the python shell
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt --matplotlib")

;; emacs ipython notebook
(require 'ein)

;; auto-complete for ein
(setq ein:use-auto-complete-superpack t)
;; (setq ein:use-smartrep t)

;; from the doc
(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)

;; conda environment settings
(setenv "WORKON_HOME" (expand-file-name "~/miniconda3/envs"))

;; Taken from
;; https://github.com/millejoh/emacs-ipython-notebook/blob/master/lisp/zeroein.el
;; This significantly improves completion in the ipython notebook
;; buffers. Otherwise . opens a minibuffer to do the completion which
;; is really crap
;; Disabling this 2018-05-11 because spacemacs uses company (and not
;; auto-complete) by default
;; (require 'auto-complete-config)
;; (ac-config-default)
