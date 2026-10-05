#!/usr/bin/env bash

dev="$1"

if [[ -z "$dev" ]]; then
    echo "usage: off <device>   (e.g. off sda)" >&2
    exit 1
fi

[[ "$dev" != /* ]] && dev="/dev/$dev"
[[ -b "$dev" ]] || { echo "off: not a block device: $dev" >&2; exit 1; }

# resolve to the whole disk (power-off takes the drive, not a partition)
disk=$(lsblk -no PKNAME "$dev" | head -n1)
disk=${disk:-${dev#/dev/}}

# safety net: never power off anything that isn't USB
[[ "$(lsblk -drno TRAN "/dev/$disk")" == usb ]] || {
    echo "off: refusing to power off non-USB drive /dev/$disk" >&2
    exit 1
}

if [[ "$(lsblk -rno FSTYPE "$dev" | head -n1)" == "crypto_LUKS" ]]; then
    # LUKS: unmount the filesystem inside, then close the container
    mapper=$(lsblk -rno PATH "$dev" | sed -n '2p')

    if [[ -n "$mapper" ]]; then
        if lsblk -rno MOUNTPOINTS "$dev" | grep -q .; then
            udisksctl unmount -b "$mapper" || exit 1
        fi
        udisksctl lock -b "$dev" || exit 1
    fi
else
    # plain filesystem: unmount only if it's actually mounted
    if lsblk -rno MOUNTPOINTS "$dev" | grep -q .; then
        udisksctl unmount -b "$dev" || exit 1
    fi
fi

udisksctl power-off -b "/dev/$disk"
