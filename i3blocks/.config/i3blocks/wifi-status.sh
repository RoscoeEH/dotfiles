#!/bin/bash

if nmcli radio wifi | grep -q disabled; then
    echo "APM"
    exit 0
fi

network=$(nmcli -t -f active,ssid,signal dev wifi | grep "^yes:" | head -n1)
ssid=$(printf "%s" "$network" | cut -d: -f2)
signal=$(printf "%s" "$network" | cut -d: -f3)

if [ -n "$ssid" ]; then
    echo "$ssid(${signal}%)"
else
    echo "Not connected"
fi
