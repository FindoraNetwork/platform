#!/usr/bin/env bash

# start abci
echo -n "starting abci: "
abci_validator_node >> $LEDGER_DIR/fullnode.log 2>&1  &
sleep 2
pgrep -f abci_validator_node | tr "\n" " "
echo

# start node
echo -n "starting node: "
tendermint node >> $LEDGER_DIR/consensus.log 2>&1  &
sleep 2
pgrep -f "tendermint node.*" | tr "\n" " "
echo
