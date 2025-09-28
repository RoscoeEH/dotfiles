#!/bin/sh
# Toggle keyboard backlight between 0 and 50

# Get current keyboard brightness
current=$(dbus-send --type=method_call --print-reply=literal --system \
    --dest='org.freedesktop.UPower' \
    '/org/freedesktop/UPower/KbdBacklight' \
    'org.freedesktop.UPower.KbdBacklight.GetBrightness' \
    | awk '{print $2}')

# Decide new value
if [ "$current" -gt 0 ]; then
    new=0
else
    new=50
fi

# Set the new brightness
dbus-send --type=method_call --print-reply=literal --system \
    --dest='org.freedesktop.UPower' \
    '/org/freedesktop/UPower/KbdBacklight' \
    'org.freedesktop.UPower.KbdBacklight.SetBrightness' \
    "int32:${new}"

