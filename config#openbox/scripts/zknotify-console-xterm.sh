#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

xterm -geometry 50x10 \
      -T 'zknotify-console' \
      -ah -bc -fa 'Terminus' -fs 14 \
      -e $DIR/zknotify-console.sh
