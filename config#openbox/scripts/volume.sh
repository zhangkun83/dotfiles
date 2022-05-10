#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
get_status() {
  volume=$(pactl -- get-sink-volume 0 | grep -o "[0-9]*%" | head -1)
  muted=$(pactl get-sink-mute 0)
}

unmute() {
  pactl set-sink-mute 0 0
}

get_status

if [ "$1" == "up" ]; then
  pactl set-sink-volume 0 +5%
  unmute
elif [ "$1" == "down" ]; then
  pactl set-sink-volume 0 -5%
  unmute
elif [ "$1" == "mute" ]; then
  pactl set-sink-mute 0 toggle
fi

get_status

if [ "$muted" == "Mute: yes" ]; then
  $DIR/zknotify.sh "Audio muted"
else
  $DIR/zknotify.sh "Audio volume: $volume"
fi
