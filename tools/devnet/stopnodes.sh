#!/usr/bin/env bash
RED='\033[31m'
GRN="\033[32m"
NC='\033[0m'

# stop all nodes
abcis=`pgrep -f findorad`
if ! [ -z "$abcis" ]
then
    echo -n "killed nodes: "
    for pid in $abcis
    do
        kill $pid
        echo -en "$pid "
    done
    echo
fi
exit 0
