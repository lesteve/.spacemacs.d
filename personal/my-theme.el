(zenburn-with-color-variables
  (custom-theme-set-faces
   'zenburn
   ;; Remove flymake squigly lines which prevents from reading the
   ;; code. Use different background instead.
   `(flymake-errline ((t (:underline nil))))
   `(flymake-warnline ((t (:underline nil))))))

