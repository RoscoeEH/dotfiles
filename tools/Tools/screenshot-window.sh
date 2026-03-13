#!/bin/bash
grim -g "$(swaymsg -t get_tree | jq -r '.. | objects | select(.focused==true).rect | "\(.x),\(.y) \(.width)x\(.height)"')" ~/Pictures/Screenshots/screenshot_$(date +%F_%T).png
