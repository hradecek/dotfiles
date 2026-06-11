#!/bin/sh
# Mirror River's focused tag (1-9) into a state file, continuously.
#
# ristate must stay connected: one-shot `ristate` invocations return stale,
# replayed river-status events, so we run it long-lived here (started once from
# the River init) and overwrite the state file on every focus change. tags.sh
# reads that file to cycle prev/next instantly, and it stays correct no matter
# how the tag was selected -- keyboard, Waybar click, anything.

STATE="${XDG_RUNTIME_DIR:-/tmp}/river-focused-tag"

# Reap a watcher left by a previous init (River reload re-runs init). Killing
# ristate by exact process name tears down the whole pipeline via EOF; -x
# avoids the -f footgun of matching this script's own command line.
pkill -x ristate 2>/dev/null

ristate -t 2>/dev/null \
    | jq --unbuffered -r 'if (.tags // {} | length) > 0 then (.tags | to_entries[0].value[0]) else empty end' \
    | while IFS= read -r t; do printf '%s' "$t" > "$STATE"; done
