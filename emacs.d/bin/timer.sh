#!/bin/bash
if [[ "$#" -lt 2 ]]; then
    echo "Usage: timer.sh <delay> <message>"
    exit 1;
fi
echo "Now is $(date)"
echo "Sleeping for $1 ..."
sleep $1
shift
zenity --info --text="$*" --icon-name=alarm
