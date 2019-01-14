#!/bin/bash
# Switch wallpaper at every new hour

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

LAST_SWITCH_HOUR=""

while :
do
    CURRENT_HOUR="$(date +%Y%m%d%H)"
    if [[ "$LAST_SWITCH_HOUR" != "$CURRENT_HOUR" ]]; then
        "$DIR/prepare-wallpaper.sh"
        LAST_SWITCH_HOUR="$CURRENT_HOUR"
    fi
    sleep 60
done
