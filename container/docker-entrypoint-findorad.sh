#!/bin/bash

export PORT=8080
export RPCURL=http://127.0.0.1:8545
export NETWORK=PRINET
export MODE=ONLINE


if [ ! -z $ROSETTA ]; then 
    if [ $ROSETTA == true ]; then
        if [ ! -z $ROSETTA_PORT ]; then 
            export PORT=$ROSETTA_PORT
        fi

        if [ ! -z $ROSETTA_RPCURL ]; then 
            export RPCURL=$ROSETTA_RPCURL
        fi

        if [ ! -z $ROSETTA_NETWORK ]; then 
            export NETWORK=$ROSETTA_NETWORK
        fi
        
        if [ ! -z $ROSETTA_MODE ]; then 
            export MODE=$ROSETTA_MODE
        fi
        nohup findora-rosetta run > /dev/null &


    fi
fi


set -e

exec findorad "$@"