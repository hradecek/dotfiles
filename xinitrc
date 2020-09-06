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

xrand --dpi 192

# Set left cursor
xsetroot -cursor_name left_ptr &

# Set a random wallaper
feh --bg-scale --recursive --randomize ~/Pictures/Wallpapers/Selected/* &

# Startup processes
mpd &
picom -cCGfF -o 0.38 -O 200 -I 200 -t 0 -l 0 -r 3 -D2 -m 0.88 &

# Run WM
exec i3

