#!/usr/bin/env bash

swayidle -w \
	before-sleep "$HOME/.config/hypr/scripts/lock.sh" \
	timeout 300 "$HOME/.config/hypr/scripts/lock.sh & sleep 0.1 && hyprctl dispatch dpms off" \
	resume 'hyprctl dispatch dpms on' \
	timeout 600 'systemctl suspend' \
	resume 'hyprctl dispatch dpms on'
