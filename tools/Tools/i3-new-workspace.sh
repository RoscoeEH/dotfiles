#!/usr/bin/env bash
set -euo pipefail

used=$(
  i3-msg -t get_workspaces |
    jq -r '.[].num' |
    sort -n
)

next_ws=$(
  for ws in $(seq 1 10); do
    if ! grep -qx "$ws" <<< "$used"; then
      echo "$ws"
      break
    fi
  done
)

[ -n "$next_ws" ] || exit 0

case "${1:-switch}" in
  switch)
    i3-msg "workspace number $next_ws"
    ;;
  move)
    i3-msg "move container to workspace number $next_ws; workspace number $next_ws"
    ;;
  *)
    echo "usage: $0 [switch|move]" >&2
    exit 2
    ;;
esac
