#!/bin/bash

STATE_FILE="/tmp/idle-inhibit.lock"

if [ -f "$STATE_FILE" ]; then
    rm "$STATE_FILE"
    pkill -f "systemd-inhibit.*idle"
    notify-send "Idle inhibit OFF"
    pkill -RTMIN+6 i3blocks
else
    touch "$STATE_FILE"
    systemd-inhibit --what=idle --why="Manual Waybar toggle" sleep infinity &
    notify-send "Idle inhibit ON"
    pkill -RTMIN+6 i3blocks
fi
