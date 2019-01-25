#!/bin/bash
CLASSNAME=$1
STEP=$2
y=0
for windowId in `xdotool search --onlyvisible --classname "$CLASSNAME"`; do
    # Un-maximize
    wmctrl -ir $windowId -b remove,maximized_vert,maximized_horz
    xdotool windowmove $windowId x $y windowraise $windowId
    ((y+=$STEP))
done
