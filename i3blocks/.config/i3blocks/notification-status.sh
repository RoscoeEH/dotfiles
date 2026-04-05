#!/bin/bash

if dunstctl is-paused | grep -q true; then
    echo "DND"
else
    count=$(dunstctl count 2>/dev/null | grep -i history | grep -o '[0-9]\+')
    echo "${count:-0}"
fi
