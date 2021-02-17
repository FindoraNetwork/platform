#!/usr/bin/env bash
RED='\033[31m'
GRN="\033[32m"
NC='\033[0m'

# paths
DEVNET="$LEDGER_DIR/devnet"

# get params if provided
V="3"
N="1"
if [ ! -z "$1" ] && [ ! -z "$2" ]; then
    V=$1
    N=$2
fi

# clean nodes
rm -rf $DEVNET/

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
