#!/bin/bash
# Reads messages from zknotify-console-pipe and displays them in a
# terminal window.  Exits if no message has been read for $TIMEOUT seconds.

ZKNOTIFY_CONSOLE_PIPE="$HOME/.config/openbox/runtime/zknotify-console-pipe"
TIMEOUT=10
while true
do
    # Use "<>" instead of "<" to open the FIFO for both read and write
    # at the same time to prevent "read" blocking on open(). Otherwise
    # "-t" won't work.
    if read -t 10 line <>"$ZKNOTIFY_CONSOLE_PIPE"; then
        echo "$line"
    else
        exit
    fi
done
