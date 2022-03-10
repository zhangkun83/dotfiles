#!/bin/bash
get_status() {
  volume=$(pactl -- get-sink-volume 0 | grep -o "[0-9]*%" | head -1)
  muted=$(pactl get-sink-mute 0)
}

unmute() {
  pactl set-sink-mute 0 0
}

get_status

if [ "$1" == "up" ]; then
  pactl set-sink-volume 0 +10%
  unmute
elif [ "$1" == "down" ]; then
  pactl set-sink-volume 0 -10%
  unmute
elif [ "$1" == "mute" ]; then
  pactl set-sink-mute 0 toggle
fi

get_status

if [ "$muted" == "Mute: yes" ]; then
  notify-send -i audio-volume-muted "Muted"
else
  notify-send -i multimedia-volume-control "$volume"
fi

