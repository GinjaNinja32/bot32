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
data="$(wget -4 --header="Accept-Language: en-gb, en;q=0.7" --header="User-Agent: $useragent" -qT 10 -O - --save-headers $URL)"

html="$(echo "$data" | grep -aEA999999 '^\s*$')"

charset="$(echo "$data" | grep -aE 'Content-Type:.*charset=.*' | head -n1 | sed -r 's/Content-Type:.*charset=(\S+).*/\1/g')"
if [[ "$charset" != "" && "$charset" != "utf8" ]]; then
	html="$(echo "$html" | iconv -f "$charset" -t utf8 -)"
fi

echo "$html" | while read_dom; do
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
