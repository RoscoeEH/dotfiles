#!/usr/bin/env bash

set -euo pipefail

if [[ $# -ne 1 ]]; then
    echo "Usage: $0 <duration>"
    echo "Examples:"
    echo "  $0 01h40m"
    echo "  $0 30m"
    echo "  $0 2h"
    echo "  $0 1h15m30s"
    exit 1
fi

duration="$1"

# Accept combinations such as:
#   2h
#   40m
#   01h40m
#   1h15m30s
if [[ ! "$duration" =~ ^([0-9]+h)?([0-9]+m)?([0-9]+s)?$ ]] || [[ -z "$duration" ]]; then
    echo "Invalid duration: $duration"
    echo "Use a format such as 01h40m, 30m, 2h, or 1h15m30s."
    exit 1
fi

hours=0
minutes=0
seconds=0

if [[ "$duration" =~ ([0-9]+)h ]]; then
    hours="${BASH_REMATCH[1]}"
fi

if [[ "$duration" =~ ([0-9]+)m ]]; then
    minutes="${BASH_REMATCH[1]}"
fi

if [[ "$duration" =~ ([0-9]+)s ]]; then
    seconds="${BASH_REMATCH[1]}"
fi

# 10# forces decimal interpretation, so values such as 08 don't get
# interpreted as invalid octal numbers by Bash.
total_seconds=$((10#$hours * 3600 + 10#$minutes * 60 + 10#$seconds))

if (( total_seconds <= 0 )); then
    echo "Duration must be greater than zero."
    exit 1
fi

echo "Shutdown timer started for: $duration"
echo "Press Ctrl+C to cancel."
echo

while (( total_seconds > 0 )); do
    h=$((total_seconds / 3600))
    m=$(((total_seconds % 3600) / 60))
    s=$((total_seconds % 60))

    printf "\rTime remaining: %02d:%02d:%02d" "$h" "$m" "$s"

    sleep 1
    ((total_seconds--))
done

printf "\rTime remaining: 00:00:00\n"
echo "Timer finished. Shutting down..."

systemctl poweroff
