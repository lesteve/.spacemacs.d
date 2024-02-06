#!/bin/bash

set -e

emacs_version=$(emacs --version | head -n1 | perl -pe 's@\D+([\d.]+)@\1@')
file_to_patch=$(find ~/.emacs.d/elpa/$emacs_version -name calfw-org.el)
cp ${file_to_patch}{,.bak}

wget https://github.com/kiwanami/emacs-calfw/pull/134.patch -O /tmp/calfw-org.el.patch 
patch ${file_to_patch} /tmp/calfw-org.el.patch

