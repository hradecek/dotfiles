#!/bin/sh
#=============================================================================#
# File:   ~/.xinitrc                                                          #
# Author: hradecek <ivohradek@gmail.com>                                      #
#=============================================================================#
XRESOURCES="$HOME/.Xresources"

if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

# Merge user resources
[[ -f $XRESOURCES && -r $XRESOURCES ]] && xrdb -merge $XRESOURCES


# Set left cursor
xsetroot -cursor_name left_ptr &

# Start urxvt daemon
urxvtd -q -o -f

if [[ $HOSTNAME -eq "Antares" ]]; then
    xrandr --output eDP1 --mode 1600x900 --rotate normal --output DP2-2 --auto --right-of eDP1
    feh --bg-scale --recursive --randomize ~/Pictures/Wallpapers/Selected/* --bg-scale --recursive --randomize ~/Pictures/Wallpapers/Selected/* &
    hexchat &
else
    # Set random wallpaper
    feh --bg-scale --recursive --randomize ~/Pictures/Wallpapers/Selected/* &
fi

xscreensaver &

# Run WM
xmonad &
wait $!
