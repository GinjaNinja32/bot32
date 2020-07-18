#! /bin/bash

case $1 in
	search)
		term=$(echo $2 | sed 's/ /+/g')
		curl -s "https://store.steampowered.com/search/?cc=$3&term=$term" |
				grep -Po '(?<=data-ds-appid=")[0-9]+(?=")' |
				head -1
		;;
	info)
		curl -s "https://store.steampowered.com/api/appdetails/?appids=$2&cc=$3"
		;;
	*)
		echo "What?"
		;;
esac
