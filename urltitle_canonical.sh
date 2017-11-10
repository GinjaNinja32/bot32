#! /bin/bash

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

wget -4 --header="Accept-Language: en-gb, en;q=0.7" --header="User-Agent: $useragent" -qT 10 -O - "$URL" | readit
