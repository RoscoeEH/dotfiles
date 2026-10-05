#!/usr/bin/env bash

dev="$1"

if [[ -z "$dev" ]]; then
    echo "usage: mnt <device>   (e.g. mnt /dev/sdb1)" >&2
    exit 1
fi

# allow shorthand: mnt sdb1
[[ "$dev" != /* ]] && dev="/dev/$dev"

[[ -b "$dev" ]] || { echo "mnt: not a block device: $dev" >&2; exit 1; }

if [[ "$(lsblk -rno FSTYPE "$dev" | head -n1)" == "crypto_LUKS" ]]; then
    # LUKS-encrypted: line 2 of lsblk is the unlocked mapper device,
    # empty if it's still locked
    mapper=$(lsblk -rno PATH "$dev" | sed -n '2p')

    if [[ -z "$mapper" ]]; then
        # still locked -> unlock (prompts for the passphrase)
        udisksctl unlock -b "$dev" || exit 1
        mapper=$(lsblk -rno PATH "$dev" | sed -n '2p')
    fi

    udisksctl mount -b "$mapper"
else
    # plain filesystem: same as your old alias
    udisksctl mount -b "$dev"
fi
