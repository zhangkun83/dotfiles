#!/bin/bash
# To be used with xfce4-genmon-plugin
ZIPCODE="$1"
LABEL="$2"
FULLRESULT=$(curl wttr.in/$ZIPCODE?1ATm)

# TEMP format is either "19 °C" or "+18(19) °C"
TEMP=$(echo "$FULLRESULT" | grep -o '+\?[0-9]*\(([0-9]*)\)\? °C' | grep -o '[0-9]*' | head -1)

echo "<txt><span size='x-small'>$LABEL </span>$TEMP°</txt>"
echo -e "<tool><span font='mono' size='x-small'>Updated on $(date)\n$FULLRESULT</span></tool>"
