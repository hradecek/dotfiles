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

# Start MPD
[[ ! -s ~/.config/mpd/pid ]] && mpd

# Set/Restore wallpaper
nitrogen --restore

# Set left cursor
xsetroot -cursor_name left_ptr &

# Start urxvt daemon
urxvtd -q -o -f

# Run WM
xmonad &
wait $!
