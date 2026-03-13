#!/bin/bash
grim -g "$(slurp)" - | tee ~/Pictures/Screenshots/screenshot_$(date +%F_%T).png | wl-copy
