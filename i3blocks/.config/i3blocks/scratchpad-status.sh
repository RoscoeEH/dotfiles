#!/bin/bash

i3-msg -t get_tree | jq "[.. | objects | select(.scratchpad_state != null and .scratchpad_state != \"none\")] | length "

