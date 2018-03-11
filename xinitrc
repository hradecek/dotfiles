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

# Set a random wallaper
feh --bg-scale --recursive --randomize ~/Pictures/Wallpapers/Selected/* &

# Startup processes
mpd &
xscreensaver &
weechat &

# Run WM
xmonad &
wait $!
