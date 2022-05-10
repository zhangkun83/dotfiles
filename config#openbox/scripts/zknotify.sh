#!/bin/bash
# Writes messages to zknotify-pipe

PIPE_FILE="$HOME/.config/openbox/runtime/zknotify-pipe"

if [[ -p "$PIPE_FILE" ]]; then
    timeout 5 bash -c "echo '$*' >'$PIPE_FILE'" ||\
        >&2 echo "zknotify: timed out writing '$*' to '$PIPE_FILE'"
fi
