#!/bin/bash
# Switch wallpaper at every new hour

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

sleep 10
LAST_SWITCH_HOUR="$(date +%Y%m%d%H)"
"$DIR/prepare-wallpaper.sh"
while :
do
    CURRENT_HOUR="$(date +%Y%m%d%H)"
    if [[ "$LAST_SWITCH_HOUR" != "$CURRENT_HOUR" ]]; then
        "$DIR/prepare-wallpaper.sh"
        LAST_SWITCH_HOUR="$CURRENT_HOUR"
    fi
    sleep 60
done
