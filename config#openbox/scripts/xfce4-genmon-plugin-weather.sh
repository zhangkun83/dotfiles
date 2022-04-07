#!/bin/bash
# To be used with xfce4-genmon-plugin
location="$1"
label="$2"
encoded_location=$(python3 -c "import urllib.parse; print(urllib.parse.quote(input()))" <<< "$location")

# Current weather condition (first 8 lines) is almost always outdated.
# Skip them and only use forecasts
fullresult=$(curl wttr.in/$encoded_location?1ATm | tail +8)

# Capture the forecast temps. 4 values to be filled in the array.
# TEMP format is either "19 °C" or "+18(19) °C"
# Remove the number in the parenthesis (I don't know what it means).
temps=($(echo "$fullresult" | grep -o '+\?[0-9]*\(([0-9]*)\)\? °C' | sed 's/([0-9]*)//g' | grep -o '[0-9]*'))

# Widget displays forecasted weather based on current time
current_hour=$(date +%H)
if [[ $current_hour -lt 7 ]]; then
    temp=${temps[3]}
    time_label="夜"
elif [[ $current_hour -lt 12 ]]; then
    temp=${temps[0]}
    time_label="朝"
elif [[ $current_hour -lt 17 ]]; then
    temp=${temps[1]}
    time_label="午"
elif [[ $current_hour -lt 22 ]]; then
    temp=${temps[2]}
    time_label="夕"
else
    temp=${temps[3]}
    time_label="夜"
fi

echo -e "<txt><span size='x-small'>$label\n$time_label </span>$temp°</txt>"
echo -e "<tool><span font='Liberation Mono' size='x-small'>Updated at $(date +%H:%M)\n$fullresult</span></tool>"
