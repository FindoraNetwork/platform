#!/usr/bin/env bash
RED='\e[31m'
GRN="\e[32m"
NC='\033[0m'

# stop all abci nodes
abcis=`pgrep -f abci_validator_node`
if ! [ -z "$abcis" ]
then
    echo -n "killed abci: "
    for pid in $abcis
    do
        kill -9 $pid
        echo -en "${RED}$pid${NC} "
    done
    echo
fi

# stop all tendermint nodes
nodes=`pgrep -f "tendermint node.*"`
if ! [ -z "$abcis" ]
then
    echo -n "killed node: "
    for pid in $nodes
    do
        kill -9 $pid
        echo -en "${RED}$pid${NC} "
    done
    echo
fi
