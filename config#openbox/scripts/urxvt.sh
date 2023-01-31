#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

urxvtc --inputMethod ibus --font "xft:DejaVu Sans Mono:size=11" --letterSpace -1 -sr -bc -st\
      -icon "${DIR}/terminal.svg" "$@"
