#!/bin/bash

# Target directory defaults to current directory if none is provided
TARGET_DIR="${1:-.}"

# Check if the target is a valid directory
if [ ! -d "$TARGET_DIR" ]; then
    echo "Error: '$TARGET_DIR' is not a valid directory." >&2
    exit 1
fi

total_lines=0

# Recursively find all files in the directory
while IFS= read -r -d '' file; do
    # Verify the file is plaintext (text/* or application/x-empty)
    file_mime=$(file --mime-type -b "$file")
    
    if [[ "$file_mime" == text/* || "$file_mime" == "application/x-empty" ]]; then
        # Count lines using wc, handling files lacking a trailing newline via awk
        lines=$(awk 'END {print NR}' "$file")
        echo "$lines lines: $file"
        total_lines=$((total_lines + lines))
    fi
done < <(find "$TARGET_DIR" -type f -print0)

echo "========================================="
echo "Total lines across all plaintext files: $total_lines"
