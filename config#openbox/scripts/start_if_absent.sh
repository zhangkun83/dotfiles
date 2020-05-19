#!/bin/bash
EXEC="$1"
shift

RESULT=$(ps -u $USER -o command | grep -F "$EXEC" | grep -v '\(^grep \)\|\(start_if_absent\.sh \)')

function message {
    echo "$1"
    notify-send "$1"
}

if [ -z "$RESULT" ]; then
    $@ &
    message "start_if_absent: $EXEC started"
    return 0
else
    message "start_if_absent: $EXEC already running"
    return 1
fi
