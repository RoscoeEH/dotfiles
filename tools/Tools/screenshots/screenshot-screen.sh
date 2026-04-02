#!/bin/bash
file=~/Pictures/Screenshots/screenshot_$(date +%F_%T).png

maim "$file" && \
xclip -selection clipboard -t image/png -i "$file"
