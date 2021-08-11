#!/usr/bin/env bash

RED='\033[31m'
GRN="\033[32m"
NC='\033[0m'

# paths
DEVNET="$LEDGER_DIR/devnet"

# show nodes
nodes=`ls -l $DEVNET | grep node  | awk '(NR>0){print $9}' | sort -V`
for node in $nodes
do
    abci=`pgrep -f "abci_validator_node $DEVNET/$node$" | tr "\n" " " | xargs echo -n`
    if ! [ -z "$abci" ]
    then
        echo -n "$node: "
        echo -e "(${GRN}$abci${NC})"
    fi
done
