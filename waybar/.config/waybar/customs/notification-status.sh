#!/bin/bash

dnd=$(swaync-client --get-dnd 2>/dev/null)

if [ "$dnd" = "true" ]; then
    echo '{"text": "Do Not Disturb", "class": "dnd"}'
else
    count=$(swaync-client --count 2>/dev/null)
    echo "{\"text\": \"$count\"}"
fi
