#!/usr/bin/env bash

# stop all abci nodes
abcis=`pgrep -f abci_validator_node`
for pid in $abcis
do
    kill -9 $pid
    echo "killed abci: $pid"
done

# stop all tendermint nodes
nodes=`pgrep -f "tendermint node.*"`
for pid in $nodes
do
    kill -9 $pid
    echo "killed node: $pid"
done
