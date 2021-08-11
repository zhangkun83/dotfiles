#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

xterm -geometry 40x10+1000+1000 -bc -fa 'Terminus' -fs 12 -e bash "$DIR/hibernate.sh"

