#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
$DIR/zknotify.sh "The time is $(date +"%I:%M %p")."
