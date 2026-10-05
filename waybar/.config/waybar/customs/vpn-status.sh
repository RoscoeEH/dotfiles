#!/bin/bash

# Count the number of active connections starting with wg
wg_count=$(nmcli -t -f NAME,STATE connection show --active | grep -c "^wg")

if [ "$wg_count" -eq 1 ]; then
    # Find and echo the exact name of the active wg connection
    nmcli -t -f NAME connection show --active | grep "^wg"
elif [ "$wg_count" -gt 1 ]; then
    echo "Error"
else
    echo "Disconnected"
fi
