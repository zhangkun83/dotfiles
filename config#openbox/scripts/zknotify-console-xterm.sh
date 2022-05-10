#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

xterm -geometry 40x10 \
      -T 'zknotify-console' \
      -bc -fa 'Terminus' -fs 12 \
      -e $DIR/zknotify-console.sh
