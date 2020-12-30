#!/bin/bash
EXEC="$1"
shift

RESULT=$(ps -u $USER -o command | grep -F "$EXEC" | grep -v '\(^grep \)\|\(start_if_absent\.sh \)')

function message {
    echo "start_if_absent: $1"
    notify-send -i system-run "$1"
}

if [ -z "$RESULT" ]; then
    $@ &
    message "$EXEC started"
    exit 0
else
    message "$EXEC already running"
    exit 1
fi
