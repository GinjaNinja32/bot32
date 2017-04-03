#! /bin/bash

http get https://translate.google.com | grep -Eo '<select id=gt-tl name=tl class="jfk-button jfk-button-standard nje" tabindex=0>.*</select>' | sed -re 's/<option (SELECTED )?value=/\n/g' -e 's#</(option|select)>##g' | tail -n +2 | sed 's|>|\t|g'

