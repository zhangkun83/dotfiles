#!/bin/bash
# Writes messages to zknotify-pipe

PIPE_FILE="$HOME/.config/openbox/runtime/zknotify-pipe"

if [[ -p "$PIPE_FILE" ]]; then
    echo "$*" >"$PIPE_FILE"
fi
