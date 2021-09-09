#!/bin/bash
primary_output=$(xrandr | grep primary | awk '{print $1}')
case $1 in
    "night")
        xrandr --output $primary_output --gamma 1:1:1 --brightness 1 &&\
            redshift -PO 4500 &&\
            notify-send -i display "Color mode: Night" ;;
    "outdoor")
        xrandr --output $primary_output --gamma 0.5:0.5:0.5 --brightness 1.3 &&\
            notify-send -i display "Color mode: Outdoor / High-contrast" ;;
    "normal")
        xrandr --output $primary_output --gamma 1:1:1 --brightness 1 &&\
            redshift -PO 5800 &&\
            notify-send -i display "Color mode: Normal" ;;
esac
