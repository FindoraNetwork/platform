#!/usr/bin/env bash

# env
source tools/devnet/env.sh || exit 1

# get params if provided
V="1"
N="1"
if [ ! -z "$1" ] && [ ! -z "$2" ]; then
    V=$1
    N=$2
fi

# create folder if necessary
mkdir -p $DEVNET

# clean nodes
rm -rf $DEVNET/*

# setup nodes
tendermint testnet --v $V --n $N --o $DEVNET > /dev/null

# create abci dirs
nodes=`ls -l $DEVNET | grep node  | awk '(NR>0){print $9}' | sort -V`
for node in $nodes
do
    mkdir -p $DEVNET/$node/abci
done

# config nodes and abcis
script_config=$(dirname "$0")/confignodes.py
echo -n "setting $(($V+$N)) nodes: "
python3 $script_config
if [ $? -ne 0 ]; then
    echo -en "${RED}failed${NC}"
else
    echo -en "${GRN}finish${NC}"
fi
echo
