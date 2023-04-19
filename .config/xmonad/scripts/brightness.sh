#! /bin/bash
# Author: Lokesh Mohanty
# Usage: sh brightness.sh $1

tag="custom-brightness"

case $1 in
		up)
				light -A 5
				brightness="$(light -G)"
				dunstify "Brightness" "Brightness (+) : $brightness%" -h int:value:$brightness -i brightness-high -h string:x-dunst-stack-tag:$tag
				# canberra-gtk-play -i audio-volume-change -d "changevolume"
				;;
		down)
				light -U 5
				brightness="$(light -G)"
				dunstify "Brightness" "Brightness (-) : $brightness%" -h int:value:$brightness -i brightness-low -h string:x-dunst-stack-tag:$tag
				# canberra-gtk-play -i audio-volume-change -d "changevolume"
				;;
esac
