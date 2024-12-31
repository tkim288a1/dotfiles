#!/bin/bash

FOLDERS="$HOME/Dropbox/dox $HOME/gnb"

find -L -f $FOLDERS \( -name "*.pdf" -o -name "*.djvu" -o -name "*.ps" \) | sed 's|/Users/tae|~|g' | sort -f
