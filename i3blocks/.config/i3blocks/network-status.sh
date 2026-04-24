#!/bin/bash

eth_iface=$(ip route show default 2>/dev/null | awk '$5 ~ /^en/ {print $5; exit}')

if [ -n "$eth_iface" ]; then
    eth_ip=$(ip -4 -o addr show dev "$eth_iface" 2>/dev/null | awk '{print $4}' | cut -d/ -f1)
    if [ -n "$eth_ip" ]; then
        echo "Eth: $eth_ip"
        exit 0
    fi
fi

if nmcli radio wifi | grep -q disabled; then
    echo "Disconnected"
    exit 0
fi

network=$(nmcli -t -f active,ssid,signal dev wifi | grep "^yes:" | head -n1)
ssid=$(printf "%s" "$network" | cut -d: -f2)
signal=$(printf "%s" "$network" | cut -d: -f3)

if [ -n "$ssid" ]; then
    echo "WiFi: $ssid (${signal}%)"
    exit 0
fi

echo "Disconnected"
