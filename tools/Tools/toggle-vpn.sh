#!/bin/bash

if tailscale status | grep -q "Tailscale is stopped."
then
    tailscale up
    pkill -RTMIN+3 i3blocks
    echo "tailscale up"
else
    tailscale down
    pkill -RTMIN+3 i3blocks
    echo "tailscale down"
fi
