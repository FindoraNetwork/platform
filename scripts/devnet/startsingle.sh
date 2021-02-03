#!/usr/bin/env bash

RED='\033[31m'
GRN="\033[32m"
NC='\033[0m'

# paths
SINGLE="$LEDGER_DIR/single"

# start abci
echo -n "starting node: "
abci_validator_node $SINGLE >> $SINGLE/abci_validator.log 2>&1  &
sleep 2

# start node
tendermint node --home $SINGLE >> $SINGLE/consensus.log 2>&1  &

# show pids
abci=`pgrep -f abci_validator_node | tr "\n" " " | xargs echo -n`
node=`pgrep -f "tendermint node --home.*" | tr "\n" " " | xargs echo -n`
echo -e "${GRN}abci($abci) <---> node($node)${NC}"
