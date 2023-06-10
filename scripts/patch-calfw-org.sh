#!/bin/bash

set -e

file_to_patch=$(find ~/.emacs.d/ -name calfw-org.el)
cp ${file_to_patch}{,.bak}

wget https://github.com/kiwanami/emacs-calfw/pull/134.patch -O /tmp/calfw-org.el.patch 
patch ${file_to_patch} /tmp/calfw-org.el.patch

