#!/bin/bash

if [ $# -ne 1 ]; then
    echo "Usage: $0 <file-or-directory>"
    exit 1
fi

# resolve absolute path
target=$(realpath "$1")

# archive name based on basename
name=$(basename "$target")
name=${name%/}

tar -czf "$name.tar.gz" -C "$(dirname "$target")" "$name"
