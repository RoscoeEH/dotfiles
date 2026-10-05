#!/bin/bash

# Get a list of all active connections starting with wg
active_wg_vpns=$(nmcli -t -f NAME connection show --active | grep "^wg")

if [ -n "$active_wg_vpns" ]; then
    # Toggle off all matching connections line by line
    echo "$active_wg_vpns" | while read -r vpn; do
        nmcli connection down "$vpn"
    done
else
    ~/Tools/start_default_vpn.sh
fi
