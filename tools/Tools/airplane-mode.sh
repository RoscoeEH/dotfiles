#!/usr/bin/env bash

wifi_state=$(nmcli -t -f WIFI radio)

bt_raw=$(bluetoothctl show 2>/dev/null | awk -F': ' '/Powered:/ {print tolower($2); exit}')
if [[ "$bt_raw" == "yes" ]]; then
    bt_state="enabled"
elif [[ "$bt_raw" == "no" ]]; then
    bt_state="disabled"
else
    bt_state="unknown"
fi

if [[ "$wifi_state" == "disabled" || "$bt_state" == "disabled" ]]; then
    nmcli radio all on
    bluetoothctl power on >/dev/null
else
    nmcli radio all off
    bluetoothctl power off >/dev/null
fi
