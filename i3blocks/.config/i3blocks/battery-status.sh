#!/bin/bash

info=$(acpi -b)

status=$(echo "$info" | awk -F', ' '{gsub(/Battery [0-9]+: /,"",$1); print $1}')
percent=$(echo "$info" | grep -o '[0-9]\+%' | tr -d '%')

# Short status label
case "$status" in
    "Charging") s="CHR" ;;
    "Discharging") s="DCR" ;;
    "Not charging") s="NCR" ;;
    *) s="$status" ;;
esac

# Output text
echo "Batt: $s $percent%"
echo ""

# Color logic
if [ "$percent" -le 15 ]; then
    echo "#ff5c5c"
elif [ "$percent" -le 30 ]; then
    echo "#ff8d5c"
else
    echo "#5a8ffa"
fi
