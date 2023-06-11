#!/usr/bin/env bash

swayidle -w \
	before-sleep "$HOME/.config/hypr/scripts/lock.sh" \
	timeout 600 "$HOME/.config/hypr/scripts/lock.sh & sleep 0.1 && hyprctl dispatch dpms off" \
	resume 'hyprctl dispatch dpms on' \
	timeout 1800 'systemctl suspend' \
	resume 'hyprctl dispatch dpms on'
