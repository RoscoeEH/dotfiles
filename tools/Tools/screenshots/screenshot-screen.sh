#!/bin/bash
grim - | tee ~/Pictures/Screenshots/screenshot_$(date +%F_%T).png | wl-copy
