#!/usr/bin/env bash

print_help() {
	echo "Usage: script_name [option]"
	echo "Options:"
	echo "  up    - Increase volume by 5%"
	echo "  down  - Decrease volume by 5%"
	echo "  mute  - Toggle mute"
}

get_volume() {
	amixer get Master | grep '%' | head -n 1 | cut -d '[' -f 2 | cut -d '%' -f 1
}

is_muted() {
	amixer get Master | grep '%' | grep -oE '[^ ]+$' | grep off >/dev/null
}

send_notification() {
	icon_sound="audio-volume-high"
	icon_mute="audio-volume-muted"

	if is_muted; then
		dunstify -i $icon_mute -t 1500 -r 2593 -u normal "Muted"
	else
		volume=$(get_volume)

		bar=$(seq -s "" $(($volume / 5)) | sed 's/[0-9]//g')
		dunstify -i $icon_sound -t 1500 -r 2593 -u normal "   $bar"
	fi
}

case "$1" in
up)
	amixer set Master 5%+ unmute >/dev/null
	send_notification
	;;
down)
	amixer set Master 5%- unmute >/dev/null
	send_notification
	;;
mute)
	amixer set Master toggle >/dev/null
	send_notification
	;;
*)
	# Unknown option
	echo "Invalid option: $1"
	print_help
	exit 1
	;;
esac
