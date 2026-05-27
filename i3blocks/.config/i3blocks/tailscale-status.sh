#!/bin/bash

if tailscale status | grep -q "Tailscale is stopped."; then
    echo "DOWN"
    echo ""
    echo "#5a8ffa"
elif tailscale status | grep -q "roscoe-btq-laptop"; then
    echo "UP"
    echo ""
    echo "#a442f5"
else
    echo "ERROR"
    echo ""
    echo "#ff0000"
fi

