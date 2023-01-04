#!/bin/bash
cd /root/

export PORT=8080
export RPCURL=http://127.0.0.1:8545
export NETWORK=PRINET

if [ ! -n "$MODE" ]; then
    export MODE=ONLINE
fi

nohup /root/findora-rosetta run >/root/findora-rosetta.log 2>&1 &
/bin/bash -c "while true;do echo hello;sleep 50000;done"
