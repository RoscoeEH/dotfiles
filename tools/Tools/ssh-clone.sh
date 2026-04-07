#!/usr/bin/env bash

set -euo pipefail

if [ $# -ne 1 ]; then
    echo "Usage: $0 <github-https-url>"
    exit 1
fi

url="$1"

# Validate it's a GitHub HTTPS URL
if [[ "$url" != https://github.com/* ]]; then
    echo "Error: only https://github.com URLs are supported"
    exit 1
fi

# Strip protocol and convert to SSH
path="${url#https://github.com/}"
ssh_url="git@github.com:${path%.git}.git"

echo "Cloning: $ssh_url"
git clone "$ssh_url"
