
#! /bin/bash

pattern=$1
serverid=$2

grep -Ei "$pattern" logs/ahelp/ahelp-$serverid.txt > ahelp_results.txt

if [[ -s ahelp_results.txt ]]; then
	md5=$(md5sum ahelp_results.txt | grep -Eo '^[0-9a-f]{32}')
	mv ahelp_results.txt ~/www/$md5.txt

	echo -n "Search complete at https://nyx.gn32.uk/admin/$md5.txt"
else
	echo -n "No results found."
fi
