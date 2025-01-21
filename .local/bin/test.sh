#!/bin/bash

# Path to the PDF cache file
PDF_CACHE="$HOME/.cache/pdf_cache"

# Check if the cache file exists
if [[ ! -f "$PDF_CACHE" ]]; then
    echo "PDF cache file not found. Exiting."
    exit 1
fi

# Use fzf to select a file
PDF_FILE=$(cat "$PDF_CACHE" | fzf --prompt="Select a PDF: ")

# Check if a file was selected
if [ -n "$PDF_FILE" ]; then
    # Replace '~' with full path if necessary and open the file in sioyek
    sioyek "${PDF_FILE/#~/$HOME}"
else
    echo "No file selected."
fi



# #!/bin/bash

# PDF_CACHE="$HOME/.cache/pdf_cache"
# # Find and select a PDF file using fuzzy matcher and store the output in a variable
# PDF_FILE=$(cat "$PDF_CACHE" | fzf)

# # Check if a file was selected
# if [ -n "$PDF_FILE" ]; then
#     # Replace '~' with full path if necessary and open the file in sioyek
#     sioyek "${PDF_FILE/#~/$HOME}"
# else
#     echo "No file selected."
# fi





# # Predefined locations to search for PDFs
# FOLDERS=(
#     "$HOME/dox"
#     "$HOME/Projects"
#     "$HOME/tmp"
#     "$HOME/clones"
#     "$HOME/Documents"
#     "$HOME/Downloads"
#     "$HOME/gnb"
#     "$HOME/Dropbox/dox"
#     "$HOME/Dropbox/teaching"
#     "$HOME/Dropbox/tmp"
#     "$HOME/1131"
#     "$HOME/1151"
#     "$HOME/1152"
#     "$HOME/3607"
#     "$HOME/3607master"
#     "$HOME/5602"
#     "$HOME/5601"
# )

# # Filter out non-existent directories
# VALID_FOLDERS=()
# for folder in "${FOLDERS[@]}"; do
#     if [ -d "$folder" ]; then
#         VALID_FOLDERS+=("$folder")
#     fi
# done

# # Convert the valid folders array into a space-separated string
# FOLDERS_STRING=$(printf "%s " "${VALID_FOLDERS[@]}")

# # Find and select a PDF file using fuzzy matcher and store the output in a variable
# PDF_FILE=$(find -H $FOLDERS_STRING \( -name "*.pdf" -o -name "*.djvu" -o -name "*.ps" \) \
#     | sed 's|/Users/tae|~|g' \
#     | sort -f \
#     | choose)

# # Check if a file was selected
# if [ -n "$PDF_FILE" ]; then
#     # Replace '~' with full path if necessary and open the file in sioyek
#     sioyek "${PDF_FILE/#~/$HOME}"
# else
#     echo "No file selected."
# fi



# #!/bin/bash

# # Predefined locations to search for PDFs
# FOLDERS="$HOME/dox \
# $HOME/Projects \
# $HOME/tmp \
# $HOME/clones \
# $HOME/Documents \
# $HOME/Downloads \
# $HOME/gnb \
# $HOME/Dropbox/dox \
# $HOME/Dropbox/teaching \
# $HOME/Dropbox/tmp \
# $HOME/1131 \
# $HOME/1151 \
# $HOME/1152 \
# $HOME/3607 \
# $HOME/3607master \
# $HOME/5602 \
# $HOME/5601"

# # Find and select a PDF file using fuzzy matcher and store the output in a variable
# PDF_FILE=$(find -H -f "$FOLDERS" \( -name "*.pdf" -o -name "*.djvu" -o -name "*.ps" \) | sed 's|/Users/tae|~|g' | sort -f | choose)

# # Check if a file was selected
# if [ -n "$PDF_FILE" ]; then
#     # Replace '~' with full path if necessary and open the file in sioyek
#     sioyek "${PDF_FILE/#~/$HOME}"
# else
#     echo "No file selected."
# fi
