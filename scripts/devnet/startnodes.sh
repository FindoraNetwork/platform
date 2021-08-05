#!/usr/bin/env bash

RED='\033[31m'
GRN="\033[32m"
NC='\033[0m'

# paths
DEVNET="$LEDGER_DIR/devnet"

# start nodes
nodes=`ls -l $DEVNET | grep node  | awk '(NR>0){print $9}' | sort -V`
for node in $nodes
do
        ./release/bin/findorad node \
        --config $DEVNET/$node/config/config.toml \
        --ledger-dir $DEVNET/$node/abci \
        --tendermint-node-key-config-path ${DEVNET}/${node}/config/priv_validator_key.json \
        >> $DEVNET/$node/abci_validator.log 2>&1  &
done

# show nodes
for node in $nodes
do
    echo -n "$node: "
    abci=`pgrep -f "./release/bin/findorad node --config $DEVNET/$node/config/config.toml" | tr "\n" " " | xargs echo -n`
    echo -e "(${GRN}$abci${NC})"
done
