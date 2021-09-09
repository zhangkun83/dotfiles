#!/bin/bash

xterm -geometry 40x10+1000+1000 \
      -T 'Shutdown' \
      -bc -fa 'Terminus' -fs 12 \
      -e bash -c "echo Enter password to shutdown the computer; sudo poweroff"
