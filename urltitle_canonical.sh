#! /bin/bash

if [ -z "$DEBUG" ]; then
	DEBUG=false
fi

URL="$1"
if [ -n $bot32 ]; then
	useragent="bot32/$bot32 (http://github.com/GinjaNinja32/bot32; ginjaninja32+bot32@gmail.com)"
else
	useragent="bot32/unspecified (http://github.com/GinjaNinja32/bot32; ginjaninja32+bot32@gmail.com)"
fi

read_dom () {
	local IFS=\>
	read -d \< ENTITY CONTENT
}

readit() {
	link=""
	title=""
	while read_dom; do
		case "$ENTITY" in
			title)
				title="$CONTENT"
				if [[ "$link" != "" ]]; then
					echo "$link"
					echo "$title"
					exit 0
				fi
				;;
			link\ rel=\"canonical\"\ href=\"*\"*/)
				link="${ENTITY#link rel=\"canonical\" href=\"}"
				link="${link%\"*/}"

				if [[ "$link" =~ ^/.* ]]; then
					schema="${URL%%//*}"
					domain_path="${URL#*//}"
					domain="${domain_path%%/*}"
					link="$schema//$domain$link"
				fi

				if [[ "$title" != "" ]]; then
					echo "$link"
					echo "$title"
					exit 0
				fi
				;;
			*)	;;
		esac
	done

	echo "$URL"
	echo "$title"
}

data="$(wget -4 --header="Accept-Language: en-gb, en;q=0.7" --header="User-Agent: $useragent" -qT 10 -O - --save-headers $URL)"

html="$(echo "$data" | grep -aEA999999 '^\s*$')"

datatype="$(echo "$html" 2>/dev/null | file -)"
if $DEBUG; then
	echo "datatype: $datatype"
fi
if [[ "$datatype" != *text* ]]; then
	echo "datatype is not text"
	exit 0
fi

charset="$(echo "$data" | grep -aE '^$|Content-Type:.*charset=.*' | head -n1 | sed -r 's/Content-Type:.*charset=(\S+).*/\1/g')"
if $DEBUG; then
	echo "charset: $charset"
fi

if [[ "$charset" == "" ]]; then
	charset="$(echo "$html" | grep -aE '<meta [^>]*charset="?[^"]+"?>' | sed -r 's/.*charset="?([^"]+)"?>.*/\1/g' | head -n1)"
	if $DEBUG; then
		echo "charset: $charset"
	fi
fi

if [[ "$charset" == "" ]]; then
	ncharset="$(echo "$html" | uchardet)"
	if [[ "$ncharset" != "ascii/unknown" ]]; then
		charset="$ncharset"
		if $DEBUG; then
			echo "charset: $charset"
		fi
	fi
fi

if [[ "$charset" != "" && "$charset" != "utf8" ]]; then
        html="$(echo "$html" | iconv -f "$charset" -t utf8 -)"
fi

echo "$html" 2>/dev/null | readit
