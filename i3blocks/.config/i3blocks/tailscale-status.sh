#!/bin/bash

if tailscale status | grep -q "Tailscale is stopped."; then
    echo "DOWN"
    echo ""
    echo "#5a8ffa"
else
    echo "UP"
    echo ""
    echo "#a442f5"
fi

