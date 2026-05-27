#!/bin/bash

if pactl get-sink-mute @DEFAULT_SINK@ | grep -q "yes"
  then echo "MUTE"
  else pactl get-sink-volume @DEFAULT_SINK@ | head -n 1 | awk "{print \$5}"
fi
