#!/bin/bash

if dunstctl is-paused | grep -q true; then
    echo "DND"
    echo ""
    echo "#5a8ffa"   
else
    count=$(dunstctl count 2>/dev/null | grep -i history | grep -o '[0-9]\+')
    count=${count:-0}

    echo "$count"
    echo ""

    if [ "$count" -gt 0 ]; then
        echo "#FF0000"   
    else
        echo "#5a8ffa"   
    fi
fi
