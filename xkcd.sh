#! /bin/bash

num="$(http --ignore-stdin --form post https://relevant-xkcd-backend.herokuapp.com/search search="$1" | jq '.results[0].number')"

title="$(./urltitle.sh "https://xkcd.com/$num" | sed 's/^xkcd: //')"

echo "https://xkcd.com/$num - $title"
