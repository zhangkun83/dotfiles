#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
EXEC="$1"
shift

RESULT=$(ps -u $USER -o command | grep -F "$EXEC" | grep -v '\(^grep \)\|\(start_if_absent\.sh \)')

function message {
    if [[ ! "$ZK_START_IF_ABSENT_SILENT" == "true" ]]; then
        echo "start_if_absent: $1"
        $DIR/zknotify.sh "$1"
    fi
}

if [ -z "$RESULT" ]; then
    $@ &
    message "$EXEC started"
    exit 0
else
    message "$EXEC already running"
    exit 1
fi
