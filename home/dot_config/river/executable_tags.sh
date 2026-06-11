#!/bin/sh
# Cycle the focused River tag (previous / next workspace) with wrap-around.
#
# River has no built-in next/prev-tag command, and riverctl cannot read the
# focused tags. The current tag is kept in a state file by tag-watch.sh (a
# long-lived ristate mirror), so reading it here is instant and stays correct
# regardless of how the tag was selected -- keyboard, Waybar click, anything.

STATE="${XDG_RUNTIME_DIR:-/tmp}/river-focused-tag"
NUM_TAGS=9

cur=$(cat "$STATE" 2>/dev/null)
case "$cur" in
    ''|*[!0-9]*) cur=1 ;;     # fall back if the watcher hasn't written yet
esac

case "$1" in
    next) n=$((cur % NUM_TAGS + 1)) ;;
    prev) n=$((cur - 1)); [ "$n" -lt 1 ] && n=$NUM_TAGS ;;
    *)    echo "usage: $0 {next|prev}" >&2; exit 1 ;;
esac

riverctl set-focused-tags $((1 << (n - 1)))
# Optimistic write so rapid presses chain correctly before the watcher catches up.
printf '%s' "$n" > "$STATE"
