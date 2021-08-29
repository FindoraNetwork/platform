#!/usr/bin/env bash

RED='\033[31m'
GRN="\033[32m"
NC='\033[0m'

# paths
DEVNET="$LEDGER_DIR/devnet"

# show abcis and nodes
nodes=`ls -l $DEVNET | grep node  | awk '(NR>0){print $9}' | sort -V`
for node in $nodes
do
    abci=`pgrep -f "abcid $DEVNET/$node$" | tr "\n" " " | xargs echo -n`
    if ! [ -z "$abci" ]
    then
        echo -n "$node: "
        echo -en "abci(${GRN}$abci${NC}) <---> "
        node=`pgrep -f "tendermint node --home $DEVNET/$node$" | tr "\n" " " | xargs echo -n`
        echo -e "node(${GRN}$node${NC})"
    fi
done
