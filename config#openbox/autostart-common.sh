$DIR/scripts/start_if_absent.sh zknotifyd.sh $DIR/scripts/zknotifyd.sh
$DIR/scripts/start_if_absent.sh xdotool-piped.sh $DIR/scripts/xdotool-piped.sh
$DIR/scripts/start_if_absent.sh urxvtd urxvtd -q -o -f
$DIR/scripts/start_if_absent.sh xss-lock xss-lock -- $DIR/scripts/lock.sh
$DIR/scripts/start_if_absent.sh wallpaper-auto-switcher.sh $DIR/scripts/wallpaper-auto-switcher.sh || $DIR/scripts/prepare-wallpaper.sh

$DIR/scripts/start_if_absent.sh afk-logger $DIR/scripts/afk-logger ~/.afk.log

$DIR/scripts/start_if_absent.sh nm-applet nm-applet

# light-locker is the screen locker from lightdm.
# I want to use my own locker, so disable it.
# Alternatively, just "apt-get remove light-locker"
# killall light-locker

xfce4-panel &
xfce4-power-manager &

fcitx &

# For unknown reasons redshift adjustments doesn't work if run immediately.
# Delaying it.
(sleep 5; $DIR/scripts/hci-adjustments) &
