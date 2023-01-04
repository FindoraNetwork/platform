#!/bin/bash

export PORT=8080
export RPCURL=http://127.0.0.1:8545
export NETWORK=PRINET

if [ ! -n "$MODE" ]; then
    export MODE=ONLINE
fi

nohup findora-rosetta run

set -e

exec findorad "$@"