#!/bin/bash

set -e 

file_to_patch=$(find ~/.emacs.d/ -name calfw-org.el)
cp ${file_to_patch}{,.bak}
patch ${file_to_patch} scripts/calfw-org.el.patch

