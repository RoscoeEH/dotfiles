#!/usr/bin/env bash

get_used_percent() {
    local json

    json="$(
    {
        printf '%s\n' \
            '{"id":1,"method":"initialize","params":{"clientInfo":{"name":"i3blocks","version":"1"}}}' \
            '{"method":"initialized","params":{}}' \
            '{"id":2,"method":"account/rateLimits/read","params":{}}'

        sleep 1
    } |
    timeout 5 codex app-server --stdio 2>/dev/null
    )"

    jq -r '
        select(.id == 2)
        | .result.rateLimits.primary
        | select(.windowDurationMins == 300)
        | .usedPercent
    ' <<< "$json"
}

used=""

for attempt in $(seq 1 15); do
    used="$(get_used_percent)"

    if [[ -n "$used" && "$used" != "null" ]]; then
        break
    fi

    sleep 1
done

if [[ -z "$used" || "$used" == "null" ]]; then
    echo "?"
    echo ""
    echo "#ff5c5c"
    exit 0
fi

percent=$((100 - used))

# Output text
echo "$percent%"
echo ""

# Color logic
if [ "$percent" -le 15 ]; then
    echo "#ff5c5c"
elif [ "$percent" -le 30 ]; then
    echo "#ff8d5c"
else
    echo "#5a8ffa"
fi
