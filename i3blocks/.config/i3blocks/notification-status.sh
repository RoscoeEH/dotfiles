#!/bin/bash

paused=$(dunstctl is-paused)

if [ "$paused" = "true" ]; then
    echo "Do Not Disturb"
else
    count=$(dunstctl count waiting 2>/dev/null)
    echo "${count:-0}"
fi
