;; Make sure to have latest org rather than the one bundled with emacs
;; Also ox-reveal need org >= 2015... and there is an error otherwise
(require 'package)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
