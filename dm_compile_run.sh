#! /bin/bash

file=$1

if [[ $2 == "run" ]]; then
	DreamDaemon $file.dmb -invisible -ultrasafe 2>&1 | tail -n +4 | head -c 512
	exit 0
fi

cd dm/

export BYOND_SYSTEM=/home/nyx/byond/use
export PATH=/home/nyx/byond/use/bin:$PATH
export LD_LIBRARY_PATH=/home/nyx/byond/use/bin:$LD_LIBRARY_PATH

output=$(DreamMaker $file.dme 2>&1)
return=$?

if [[ $return != 0 ]]; then
	echo "$output" | tail -n +3
else
	~/timeout --just-kill --no-info-on-success --detect-hangups -h 10 -t 10 -m 102400 ../$0 $file run
fi
rm $file.*
