#! /bin/bash
# Author: sharpicx, Lokesh Mohanty
# Usage: sh volume.sh $1

tag="custom-volume"
tagMute="custom-volume-mute"
tagMicMute="custom-volume-mute-mic"

case $1 in
		up)
				pamixer --allow-boost -i 5
				volume="$(pamixer --get-volume)"
				dunstify "Volume" "Volume (+) : $volume%" -h int:value:$volume -i audio-volume-high -h string:x-dunst-stack-tag:$tag
				canberra-gtk-play -i audio-volume-change -d "changevolume"
				;;
		down)
				pamixer --allow-boost -d 5
				volume="$(pamixer --get-volume)"
				dunstify "Volume" "Volume (-) : $volume%" -h int:value:$volume -i audio-volume-low -h string:x-dunst-stack-tag:$tag
				canberra-gtk-play -i audio-volume-change -d "changevolume"
				;;
		mute)
				pamixer -t
				muted="$(pamixer --get-mute)"
				if $muted ; then
						dunstify "Volume" "Volume muted!" -i audio-volume-muted-blocking -u low -h string:x-dunst-stack-tag:$tagMute
				else
						dunstify "Volume" "Volume unmuted!" -i audio-volume-high -u low -h string:x-dunst-stack-tag:$tagMute
						canberra-gtk-play -i audio-volume-change 
				fi
				;;
		mute-mic)
				pamixer --default-source -t
				muted="$(pamixer --default-source --get-mute)"
				if $muted ; then
						dunstify "Volume" "Microphone muted!" -i audio-volume-muted-blocking -u low -h string:x-dunst-stack-tag:$tagMicMute
				else
						dunstify "Volume" "Microphone unmuted!" -i audio-volume-high -u low -h string:x-dunst-stack-tag:$tagMicMute
						canberra-gtk-play -i audio-volume-change 
				fi
				;;
esac
