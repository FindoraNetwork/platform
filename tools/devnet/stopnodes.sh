#!/usr/bin/env bash

# env
source ./tools/devnet/env.sh || exit 1

# stop one single node if specified
Node=""
if [ ! -z "$1" ]; then
    Node="node$1"
fi

# finish if devnet never created
if [ ! -d "$DEVNET" ]; then
    exit 0
fi

# stop abcis
nodes=`ls -l $DEVNET | grep node  | awk '(NR>0){print $9}' | sort -V`

killed=false
tdmt_pids=()
for node in $nodes
do
    abci=`pgrep -f "abcid $DEVNET/$node$" | tr "\n" " " | xargs echo -n`
    tdmt=`pgrep -f "tendermint node --home $DEVNET/$node$"`
    if ([ -n "$abci" ] || [ -n "$tdmt" ]) && ([ -z "$Node" ] || [ "$Node" = "$node" ]); then
        if [ "$killed" = false ]; then
            echo -n "killed abci: "
            killed=true
        fi

        kill -9 $abci
        kill -9 $tdmt | exit 0

        tdmt_pids+=("$tdmt")
        echo -en "${YEL}$abci ${NC}"
    fi
done

if [ "$killed" = true ]; then
    echo
    echo -n "killed node: "
    for tdmt in "${tdmt_pids[@]}"
    do
        echo -en "${YEL}$tdmt ${NC}"
    done
    echo
fi

exit 0
