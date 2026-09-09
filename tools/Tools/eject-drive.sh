#!/bin/bash
set -euo pipefail

DEVICE="${1:-}"

if [[ -z "$DEVICE" || ! -b "$DEVICE" ]]; then
    echo "Usage: $0 /dev/sdX"
    exit 1
fi

FSTYPE="$(lsblk -ndo FSTYPE "$DEVICE")"

if [[ "$FSTYPE" == "crypto_LUKS" ]]; then
    MAPPER="$(lsblk -nrpo NAME,TYPE "$DEVICE" | awk '$2=="crypt"{print $1; exit}')"

    if [[ -n "$MAPPER" ]]; then
        findmnt -rn -S "$MAPPER" >/dev/null 2>&1 &&
            udisksctl unmount -b "$MAPPER"

        udisksctl lock -b "$MAPPER"
    fi
else
    findmnt -rn -S "$DEVICE" >/dev/null 2>&1 &&
        udisksctl unmount -b "$DEVICE"
fi

udisksctl power-off -b "$DEVICE"
