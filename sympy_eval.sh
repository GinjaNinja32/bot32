#! /bin/bash

case "$2" in
	*__*) echo "lolnope"; ;;
	*) ~/timeout --detect-hangups --no-info-on-success -t $1 -m 102400 python sympy_eval.py "$2"; ;;
esac
