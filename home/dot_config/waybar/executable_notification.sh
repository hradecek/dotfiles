#!/bin/sh
# Mako do-not-disturb indicator for Waybar (return-type json).
if makoctl mode 2>/dev/null | grep -q '^do-not-disturb$'; then
    printf '{"text":"󰂛","class":"dnd","tooltip":"Do Not Disturb — click to enable notifications"}\n'
else
    printf '{"text":"󰂚","class":"active","tooltip":"Notifications on — click for Do Not Disturb"}\n'
fi
