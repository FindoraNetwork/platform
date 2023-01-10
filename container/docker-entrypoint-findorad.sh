#!/bin/bash

export PORT=8080
export RPCURL=http://127.0.0.1:8545
export NETWORK=PRINET

if [ ! -n "$MODE" ]; then
    export MODE=ONLINE
fi

if [ ! -z $ROSETTA ]; then 
    if [ $ROSETTA == true ]; then
        nohup findora-rosetta run > /dev/null &
    fi
fi


set -e

exec findorad "$@"