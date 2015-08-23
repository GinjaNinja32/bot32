#! /bin/bash

URL=$1

read_dom () {
	local IFS=\>
	read -d \< ENTITY CONTENT
}

d=0
wget -qT 10 -O - $URL | while read_dom; do
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
