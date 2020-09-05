#!/bin/bash

pkill polybar

while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

polybar main >> /tmp/polybar_main.log 2>&1

