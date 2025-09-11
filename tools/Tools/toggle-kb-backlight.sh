#!/bin/bash

LED_PATH="/sys/class/leds/chromeos::kbd_backlight"

current=$(cat $LED_PATH/brightness)
max=$(cat $LED_PATH/max_brightness)

if [ "$current" -eq 0 ]; then
    echo $max | sudo tee $LED_PATH/brightness
else
    echo 0 | sudo tee $LED_PATH/brightness
fi
