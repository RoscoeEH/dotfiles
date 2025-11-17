#!/bin/bash

echo "Audio sources:"
pactl list short sinks
echo ""
echo "Current source:"
pactl get-default-sink
