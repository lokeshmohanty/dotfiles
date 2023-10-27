pgrepoutput="$(pgrep -ax X\(\|org\|wayland\))"
displays="$(echo "$pgrepoutput" | grep -wo "[0-9]*:[0-9]\+" | sort -u)"

[ -z "$displays" ] && [ -d /tmp/.X11-unix ] && displays=$(cd /tmp/.X11-unix && for x in X*; do echo ":${x#X}"; done)

notify() { [ -n "$pgrepoutput" ] && for x in ${displays:-:0}; do
		export DISPLAY="$x"
		notify-send --app-name="mutt-wizard" "$1" "$2"
done ;}

new=$(find\
					"$HOME/.local/share/mail/$1/"[Ii][Nn][Bb][Oo][Xx]/new/ \
					"$HOME/.local/share/mail/$1/"[Ii][Nn][Bb][Oo][Xx]/cur/ \
					-type f -newer "$lastrun" 2> /dev/null)
newcount=$(echo "$new" | sed '/^\s*$/d' | wc -l)
case 1 in 
		$((newcount > 5)) )
			echo "$newcount new mail for $1."
			notify "New Mail!" "📬 $newcount new mail(s) in \`$1\` account."
			;;
		$((newcount > 0)) )
			echo "$newcount new mail for $1."
			[ -z "$MAILSYNC_MUTE" ] &&
			for file in $new; do
		    		# Extract and decode subject and sender from mail.
				subject="$(sed -n "/^Subject:/ s|Subject: *|| p" "$file" |
					perl -CS -MEncode -ne 'print decode("MIME-Header", $_)')"
				from="$(sed -n "/^From:/ s|From: *|| p" "$file" |
					perl -CS -MEncode -ne 'print decode("MIME-Header", $_)')"
				from="${from% *}" ; from="${from%\"}" ; from="${from#\"}"
				notify "📧$from:" "$subject"
			done
			;;
esac
