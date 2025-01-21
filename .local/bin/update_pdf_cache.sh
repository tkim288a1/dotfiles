#!/bin/bash

# Predefined locations to search for PDFs
FOLDERS=(
    "$HOME/dox"
    "$HOME/Projects"
    "$HOME/tmp"
    "$HOME/clones"
    "$HOME/Documents"
    "$HOME/Downloads"
    "$HOME/gnb"
    "$HOME/Dropbox/dox"
    "$HOME/Dropbox/teaching"
    "$HOME/Dropbox/tmp"
    "$HOME/1131"
    "$HOME/1151"
    "$HOME/1152"
    "$HOME/3607"
    "$HOME/3607master"
    "$HOME/5602"
    "$HOME/5601"
)

# Cache file for storing the list of PDF files
CACHE_FILE="$HOME/.cache/pdf_cache"

# Generate a new cache
echo "Updating file cache..."
VALID_FOLDERS=()
for folder in "${FOLDERS[@]}"; do
    [ -d "$folder" ] && VALID_FOLDERS+=("$folder")
done

FOLDERS_STRING=$(printf "%s " "${VALID_FOLDERS[@]}")

find -H $FOLDERS_STRING \( -name "*.pdf" -o -name "*.djvu" -o -name "*.ps" \) \
    | sed 's|/Users/tae|~|g' \
    | sort -f > "$CACHE_FILE"

echo "Cache updated successfully."
