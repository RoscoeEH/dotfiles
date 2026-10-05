#!/bin/bash

if tailscale status | grep -q "Tailscale is stopped."
then
    tailscale up
    echo "tailscale up"
else
    tailscale down
    echo "tailscale down"
fi
