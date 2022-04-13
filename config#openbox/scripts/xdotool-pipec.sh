#!/bin/bash
# Reads xdotool commands from a pipe and execute them.  This
# guarantees serial execution of xdotool commands so that
# CLEARMODIFIERS can work correctly when holding down a key.

# Concurrent write to the pipe is safe because "Write requests of
# {PIPE_BUF} bytes or less shall not be interleaved with data from
# other processes doing writes on the same pipe.", where PIPE_BUF is
# guaranteed to be larger than 512.

PIPE_FILE="$HOME/.config/openbox/runtime/xdotool-pipe"

if [[ -p "$PIPE_FILE" ]]; then
    echo "$*" >"$PIPE_FILE"
fi
