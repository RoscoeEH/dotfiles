#!/bin/bash


if tailscale status | grep -q "Tailscale is stopped."; then
    echo "Down"
else
    echo "Up"
fi
