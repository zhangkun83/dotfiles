#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Kill compton before starting xsecurelock. Otherwise X11 will burn CPU.
# I guess xsecurelock aggressily redraws screen for security reasons.
if killall compton; then
    export post_lock_cmd="compton"
fi

(XSECURELOCK_FONT=--courier-bold-r-normal--34------- XSECURELOCK_WANT_FIRST_KEYPRESS=1 xsecurelock; $DIR/hci-adjustments; $DIR/whattime.sh)

if [[ -n "$post_lock_cmd" ]]; then
    $DIR/zknotify.sh "Starting $post_lock_cmd"
    $post_lock_cmd &
fi

