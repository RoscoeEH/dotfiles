#!/bin/bash

wifi_state=$(nmcli -t -f WIFI radio)

if [ "$wifi_state" == "disabled" ]; then
    nmcli radio all on
else
    nmcli radio all off
fi
