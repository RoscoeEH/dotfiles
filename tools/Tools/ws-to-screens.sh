#!/usr/bin/env bash

set -u

# ----------------------------------------------------------------------
# Detect active external monitors
# ----------------------------------------------------------------------

VERTICAL_OUTPUT="$(
    xrandr --query |
        awk '
            $2 == "connected" &&
            $1 !~ /^eDP-/ &&
            match($0, /[0-9]+x[0-9]+\+[0-9]+\+[0-9]+/) {
                geometry = substr($0, RSTART, RLENGTH)
                split(geometry, pos, "+")
                split(pos[1], size, "x")

                width = size[1]
                height = size[2]

                if (height > width) {
                    print $1
                    exit
                }
            }
        '
)"

HORIZONTAL_OUTPUT="$(
    xrandr --query |
        awk '
            $2 == "connected" &&
            $1 !~ /^eDP-/ &&
            match($0, /[0-9]+x[0-9]+\+[0-9]+\+[0-9]+/) {
                geometry = substr($0, RSTART, RLENGTH)
                split(geometry, pos, "+")
                split(pos[1], size, "x")

                width = size[1]
                height = size[2]

                if (width >= height) {
                    print $1
                    exit
                }
            }
        '
)"

if [[ -z "$VERTICAL_OUTPUT" || -z "$HORIZONTAL_OUTPUT" ]]; then
    echo "Could not identify vertical and horizontal external monitors" >&2
    exit 1
fi

# ----------------------------------------------------------------------
# Explicit workspace mappings
#
# Keys are exact i3 workspace names.
# Values are "vertical" or "horizontal".
# ----------------------------------------------------------------------

declare -A WORKSPACE_MAP=(
    ["1"]="vertical"
    ["2"]="horizontal"
    ["8"]="vertical"
    ["9"]="horizontal"
)

# ----------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------

output_for_orientation()
{
    case "$1" in
        vertical)
            printf '%s\n' "$VERTICAL_OUTPUT"
            ;;
        horizontal)
            printf '%s\n' "$HORIZONTAL_OUTPUT"
            ;;
        *)
            return 1
            ;;
    esac
}

move_workspace()
{
    local workspace="$1"
    local output="$2"

    i3-msg \
        "workspace --no-auto-back-and-forth \"$workspace\"; move workspace to output \"$output\"" \
        >/dev/null
}

tree="$(i3-msg -t get_tree)"

# ----------------------------------------------------------------------
# Workspace placement
#
# Rules:
#
# 1. Explicit mapping wins.
#
# 2. Otherwise, if the workspace:
#      - has more than one tiled window
#      - has no floating windows
#      - has no meaningful horizontal split
#
#    then place it on the vertical monitor.
#
# 3. Everything else goes on the horizontal monitor.
#
# In i3 terminology:
#   splitv = windows stacked top/bottom
#   splith = windows arranged side-by-side
# ----------------------------------------------------------------------

while IFS=$'\t' read -r workspace tiled_windows floating_windows horizontal_splits
do
    if [[ -n "${WORKSPACE_MAP[$workspace]+x}" ]]; then
        orientation="${WORKSPACE_MAP[$workspace]}"
        output="$(output_for_orientation "$orientation")"
        move_workspace "$workspace" "$output"
        continue
    fi

    if (( tiled_windows > 1 &&
          floating_windows == 0 &&
          horizontal_splits == 0 )); then
        move_workspace "$workspace" "$VERTICAL_OUTPUT"
    else
        move_workspace "$workspace" "$HORIZONTAL_OUTPUT"
    fi

done < <(
    jq -r '
        def contains_window:
            ([.. | objects | select(.window? != null)] | length) > 0;

        ..
        | objects
        | select(.type? == "workspace")
        | select(.name? != "__i3_scratch")
        | . as $ws

        | [
            $ws.name,

            # Number of tiled windows.
            (
                [
                    $ws.nodes[]?
                    | ..
                    | objects
                    | select(.window? != null)
                ]
                | length
            ),

            # Number of floating windows.
            (
                [
                    $ws.floating_nodes[]?
                    | ..
                    | objects
                    | select(.window? != null)
                ]
                | length
            ),

            # Number of meaningful side-by-side splits.
            (
                [
                    $ws
                    | ..
                    | objects
                    | select(.layout? == "splith")
                    | select(
                        (
                            [
                                .nodes[]?
                                | select(contains_window)
                            ]
                            | length
                        ) > 1
                    )
                ]
                | length
            )
        ]
        | @tsv
    ' <<< "$tree"
)
