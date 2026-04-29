#!/usr/bin/env bash

next_ws=$(
  comm -23 \
    <(seq 1 10) \
    <(i3-msg -t get_workspaces | jq '.[].num' | sort -n) |
  head -n 1
)

[ -n "$next_ws" ] && i3-msg "workspace number $next_ws"
