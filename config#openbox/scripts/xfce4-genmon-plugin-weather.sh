#!/bin/bash
# To be used with xfce4-genmon-plugin
location="$1"
label="$2"
encoded_location=$(python3 -c "import urllib.parse; print(urllib.parse.quote(input()))" <<< "$location")

fullresult=$(curl wttr.in/$encoded_location?1ATm)

# TEMP format is either "19 °C" or "+18(19) °C"
TEMP=$(echo "$fullresult" | grep -o '+\?[0-9]*\(([0-9]*)\)\? °C' | grep -o '[0-9]*' | head -1)

echo "<txt><span size='x-small'>$label </span>$TEMP°</txt>"
echo -e "<tool><span font='Liberation Mono' size='x-small'>Updated at $(date +%H:%M)\n$fullresult</span></tool>"
