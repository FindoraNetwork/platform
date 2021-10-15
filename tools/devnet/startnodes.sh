#!/usr/bin/env bash

# env
source ./tools/devnet/env.sh || exit 1

# start one single node if specified
Node=""
if [ ! -z "$1" ]; then
    Node="node$1"
fi

# start abcis
nodes=`ls -l $DEVNET | grep node  | awk '(NR>0){print $9}' | sort -V`
for node in $nodes
do
if [ -z "$Node" ] || ([ ! -z "$Node" ] && [ "$Node" = "$node" ]); then
        SelfAddr=$(grep 'address' ${DEVNET}/${node}/config/priv_validator_key.json | grep -oE '[^",]{40}')
        TD_NODE_SELF_ADDR=$SelfAddr \
        RUST_LOG=$ABCI_LOG_LEVEL \
        LEDGER_DIR=$DEVNET/$node/abci \
        $BIN/abcid $DEVNET/$node >> $DEVNET/$node/abcid.log 2>&1  &
fi
done

# start nodes
for node in $nodes
do
    tendermint node --home $DEVNET/$node >> $DEVNET/$node/consensus.log 2>&1  &
done

# show abcis and nodes
for node in $nodes
do
if [ -z "$Node" ] || ([ ! -z "$Node" ] && [ "$Node" = "$node" ]); then
    echo -n "$node: "
    abci=`pgrep -f "abcid $DEVNET/$node$" | tr "\n" " " | xargs echo -n`
    echo -en "abci(${GRN}$abci${NC}) <---> "
    sleep 0.2
    node=`pgrep -f "tendermint node --home $DEVNET/$node$" | tr "\n" " " | xargs echo -n`
    echo -e "node(${GRN}$node${NC})"
fi
done
