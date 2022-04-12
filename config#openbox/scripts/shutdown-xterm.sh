#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

xterm -geometry 40x10+1000+1000 \
      -T 'Shutdown' \
      -bc -fa 'Terminus' -fs 12 \
      -xrm "xterm*iconHint: $DIR/system-shutdown.xpm" \
      -e bash -c "echo Enter password to shutdown the computer; sudo poweroff"
