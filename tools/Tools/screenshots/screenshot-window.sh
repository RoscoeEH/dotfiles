#!/bin/bash
file=~/Pictures/Screenshots/screenshot_$(date +%F_%T).png

maim -i "$(xdotool getactivewindow)" "$file" && \
xclip -selection clipboard -t image/png -i "$file"
