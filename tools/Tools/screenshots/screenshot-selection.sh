#!/bin/bash
file=~/Pictures/Screenshots/screenshot_$(date +%F_%T).png

if maim -s "$file"; then
    xclip -selection clipboard -t image/png -i "$file"
else
    rm -f "$file"
fi
