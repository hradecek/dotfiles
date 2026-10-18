#!/bin/sh
# Keyboard layout switcher + Waybar readout.
#
# River exposes no way to read the active xkb group, so an xkb `grp:` toggle
# would be invisible to the bar. Instead each switch loads a single layout via
# riverctl and records it in a state file, then signals Waybar to refresh.

LAYOUTS="sk us"                     # first one is the default
STATE="${XDG_RUNTIME_DIR:-/tmp}/river-kb-layout"
SIGNAL=8                            # matches "signal" of waybar custom/kblayout

apply() {
    riverctl keyboard-layout "$1"
    echo "$1" > "$STATE"
    pkill -RTMIN+$SIGNAL -x waybar 2>/dev/null
}

cur=$(cat "$STATE" 2>/dev/null)
[ -n "$cur" ] || cur=${LAYOUTS%% *}

case "$1" in
    init)
        apply "${LAYOUTS%% *}"
        ;;
    toggle)
        next=""
        found=""
        for l in $LAYOUTS; do
            if [ -n "$found" ]; then next=$l; break; fi
            [ "$l" = "$cur" ] && found=1
        done
        apply "${next:-${LAYOUTS%% *}}"
        ;;
    status)
        printf '{"text":"%s","tooltip":"Keyboard layout: %s","class":"%s"}\n' \
            "$(echo "$cur" | tr '[:lower:]' '[:upper:]')" "$cur" "$cur"
        ;;
    *)
        echo "usage: $0 {init|toggle|status}" >&2
        exit 1
        ;;
esac
