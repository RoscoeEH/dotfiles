#!/bin/bash

current=$(nordvpn status | grep -o "Connected")

if [ -n "$current" ]; then
    nordvpn d
else
    nordvpn c
fi
