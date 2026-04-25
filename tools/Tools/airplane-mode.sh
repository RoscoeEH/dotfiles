#!/bin/bash

wifi_state=$(nmcli -t -f WIFI radio)

if [ "$wifi_state" == "disabled" ]; then
    nmcli radio wifi on
else
    nmcli radio wifi off
fi
