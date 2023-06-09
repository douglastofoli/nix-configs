#!/usr/bin/env bash

print_help() {
	echo "Usage: $0 [--play] [--next] [--previous]"
	echo "Available options:"
	echo "  --play       Play/Pause the music"
	echo "  --next       Next song"
	echo "  --previous   Previous song"
}

if [[ $class == "playing" ]]; then
	info=$(playerctl metadata --player=spotify --format '{{artist}} - {{title}}')

	if [[ ${#info} > 40 ]]; then
		info=$(echo $info | cut -c1-40)"..."
	fi

	text="${icon} ${info}"
elif [[ $class == "paused" ]]; then
	text=$icon
elif [[ $class == "stopped" ]]; then
	text=""
fi

while [[ $# -gt 0 ]]; do
	case "$1" in
	--play)
		playerctl -p spotify play-pause
		exit
		;;
	--next)
		playerctl -p spotify next
		exit
		;;
	--previous)
		playerctl -p spotify previous
		exit
		;;
	*)
		# Unknown option
		echo "Invalid option: $1"
		print_help
		exit 1
		;;
	esac
	shift
done

class=$(playerctl metadata --player=spotify --format '{{lc(status)}}')
icon="󰓇"

if [[ $class == "playing" ]]; then
	info=$(playerctl metadata --player=spotify --format '{{artist}} - {{title}}')

	if [[ ${#info} > 40 ]]; then
		info=$(echo $info | cut -c1-40)"..."
	fi

	text="${icon} ${info}"
elif [[ $class == "paused" ]]; then
	text=$icon
elif [[ $class == "stopped" ]]; then
	text=""
fi

echo -e "{\"text\":\""$text"\", \"class\":\""$class"\"}"
