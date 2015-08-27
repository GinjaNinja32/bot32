#! /bin/bash

URL=$1
if [ -n $bot32 ]; then
	useragent="bot32/$bot32 (http://github.com/GinjaNinja32/bot32; ginjaninja32+bot32@gmail.com)"
else
	useragent="bot32/unspecified (http://github.com/GinjaNinja32/bot32; ginjaninja32+bot32@gmail.com)"
fi

read_dom () {
	local IFS=\>
	read -d \< ENTITY CONTENT
}

d=0
wget --header="User-Agent: $useragent" -qT 10 -O - $URL | while read_dom; do
	case $ENTITY in
		title)
			if [ $d -eq 0 ]; then
				echo "$CONTENT"
				d=1
			fi
			;;
		*)	;;
	esac
done
