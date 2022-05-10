#!/bin/bash
# A daemon that reads messages from zknotify-pipe and writes them to
# zknotify-console-pipe. Launch zknotify-console to display the messages
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

ZKNOTIFY_PIPE="$HOME/.config/openbox/runtime/zknotify-pipe"
ZKNOTIFY_CONSOLE_PIPE="$HOME/.config/openbox/runtime/zknotify-console-pipe"

export ZK_START_IF_ABSENT_SILENT=true

if [[ ! -p "$ZKNOTIFY_PIPE" ]]; then
    rm "$ZKNOTIFY_PIPE"
    mkfifo "$ZKNOTIFY_PIPE"
fi
while true
do
    if [[ ! -p "$ZKNOTIFY_CONSOLE_PIPE" ]]; then
        rm "$ZKNOTIFY_CONSOLE_PIPE"
        mkfifo "$ZKNOTIFY_CONSOLE_PIPE"
    fi
    if read line <"$ZKNOTIFY_PIPE"; then
        $DIR/start_if_absent.sh zknotify-console-xterm.sh $DIR/zknotify-console-xterm.sh
        echo "> $line" >"$ZKNOTIFY_CONSOLE_PIPE"
    fi
done
