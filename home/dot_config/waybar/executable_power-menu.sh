#!/bin/sh
# Fuzzel-based power menu for the Waybar power button.
choice=$(printf 'Lock\nLogout\nSuspend\nReboot\nShutdown' | fuzzel --dmenu --prompt 'power> ')
case "$choice" in
    Lock)     swaylock -f -c 0f0f0f ;;
    Logout)   riverctl exit ;;
    Suspend)  systemctl suspend ;;
    Reboot)   systemctl reboot ;;
    Shutdown) systemctl poweroff ;;
esac
