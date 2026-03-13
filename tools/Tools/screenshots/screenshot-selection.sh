#!/bin/bash
file=~/Pictures/Screenshots/screenshot_$(date +%F_%T).png
if grim -g "$(slurp)" "$file" && [ -s "$file" ]; then
    wl-copy < "$file"
else
    rm -f "$file"
fi
