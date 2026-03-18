#!/bin/bash

status=$(nordvpn status 2>/dev/null)

if echo "$status" | grep -qi "Connected"; then
    echo "$status" | grep -oP 'City: \K[^ ]+'
else
    echo "Disconnected"
fi
