#!/bin/bash
# Switch wallpaper at every new hour
# Also send a notification on every 30-minute mark.

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

sleep 10
LAST_SWITCH_HOUR="$(date +%Y%m%d%H)"
LAST_NOTIFY_MINUTE=""
"$DIR/prepare-wallpaper.sh"
while :
do
    CURRENT_MINUTE="$(date +%Y%m%d%H%M)"
    if [[ "$LAST_NOTIFY_MINUTE" != "$CURRENT_MINUTE" && "$CURRENT_MINUTE" =~ .*(30|00)$ ]]; then
        $DIR/whattime.sh
        LAST_NOTIFY_MINUTE="$CURRENT_MINUTE"
    fi
    CURRENT_HOUR="$(date +%Y%m%d%H)"
    if [[ "$LAST_SWITCH_HOUR" != "$CURRENT_HOUR" ]]; then
        "$DIR/prepare-wallpaper.sh"
        LAST_SWITCH_HOUR="$CURRENT_HOUR"
    fi
    # Make sure we check the time at least once per minute so that can
    # see the 30-minute mark.
    sleep 45
done
