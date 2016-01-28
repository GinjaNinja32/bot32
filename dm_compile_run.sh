#! /bin/bash

if [[ $1 == "run" ]]; then
	DreamDaemon dm.dmb -ultrasafe 2>&1 | tail -n +4
	exit 0
fi

export BYOND_SYSTEM=/home/nyx/byond/use
export PATH=/home/nyx/byond/use/bin:$PATH
export LD_LIBRARY_PATH=/home/nyx/byond/use/bin:$LD_LIBRARY_PATH

output=$(DreamMaker dm.dme 2>&1)
return=$?

if [[ $return != 0 ]]; then
	echo "$output" | tail -n +3
else
	~/timeout --just-kill --no-info-on-success --detect-hangups -h 10 -t 10 -m 102400 $0 run
fi
