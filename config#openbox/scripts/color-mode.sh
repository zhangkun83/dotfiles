#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
primary_output=$(xrandr | grep "\\<connected\\>" | head -1 | awk '{print $1}')
case $1 in
    "night")
        xrandr --output $primary_output --gamma 1:1:1 --brightness 1 &&\
            redshift -PO 4500 &&\
            $DIR/zknotify.sh "Color mode: Night" ;;
    "outdoor")
        xrandr --output $primary_output --gamma 0.5:0.5:0.5 --brightness 1.3 &&\
            $DIR/zknotify.sh "Color mode: Outdoor / High-contrast" ;;
    "normal")
        xrandr --output $primary_output --gamma 1:1:1 --brightness 1 &&\
            redshift -PO 5800 &&\
            $DIR/zknotify.sh "Color mode: Normal" ;;
esac
