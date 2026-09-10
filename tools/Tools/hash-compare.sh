#!/bin/bash

# Check if exactly two arguments are provided
if [ "$#" -ne 2 ]; then
    echo "Error: Please provide exactly two file paths."
    echo "Usage: $0 <file1> <file2>"
    exit 1
fi

FILE1="$1"
FILE2="$2"

# Verify that both files actually exist
if [ ! -f "$FILE1" ]; then
    echo "Error: File '$FILE1' does not exist."
    exit 1
fi

if [ ! -f "$FILE2" ]; then
    echo "Error: File '$FILE2' does not exist."
    exit 1
fi

# Compare the SHA-256 hashes
[ "$(sha256sum "$FILE1" | awk '{print $1}')" = "$(sha256sum "$FILE2" | awk '{print $1}')" ] && echo "Match" || echo "Different"
