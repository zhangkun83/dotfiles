#!/bin/bash
# To be used with xfce4-genmon-plugin
AFKRESULT=$(~/.config/openbox/scripts/afk-report ~/.afk.log today)
DAILY_HOURS=$(echo "$AFKRESULT" | tail -1 | awk '{print $2}')
echo -e "<txt><span size='x-small'>HRS</span>\n<span font='mono'>$DAILY_HOURS</span></txt>"
echo "<tool><span font='mono' size='x-small'>$AFKRESULT</span></tool>"
