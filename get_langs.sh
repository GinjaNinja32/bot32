#! /bin/bash

curl -s https://translate.google.com -H 'User-Agent: bot32/unknown' \
	| grep -Po '(?<=source_code_name:\[)[^]]+(?=\])' \
	| sed -Ee 's/\},\{/}\n{/g' \
		-e "s/\\{code:'([^']+)',name:'([^']+)'\\}/\\1\\t\\2/g" \
	| sed '/^auto\tDetect language$/d'
# | sed -re 's/<option (SELECTED )?value=/\n/g' -e 's#</(option|select)>##g' | tail -n +2 | sed 's|>|\t|g'

