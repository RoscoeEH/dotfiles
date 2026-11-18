#!/usr/bin/env bash

gap=60

# Toggle floating mode
i3-msg 'floating toggle' >/dev/null

# Only resize if the window is now floating
floating=$(i3-msg -t get_tree |
    jq -r 'recurse(.nodes[]?, .floating_nodes[]?) |
           select(.focused == true) | .floating')

if [[ "$floating" == "user_on" || "$floating" == "auto_on" ]]; then
    read -r width height < <(
        i3-msg -t get_workspaces |
            jq -r '.[] | select(.focused) |
                   "\(.rect.width) \(.rect.height)"'
    )

    i3-msg "resize set width $((width - 2 * gap)) px height $((height - 2 * gap)) px, move position center" >/dev/null
fi
