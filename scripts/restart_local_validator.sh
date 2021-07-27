#!/bin/bash

pkill -9 abci
pkill -9 tendermint

TD_NODE_SELF_ADDR=$(jq --raw-output '.address' /root/.tendermint/config/priv_validator_key.json) \
  LEDGER_DIR=/root/findora \
  ENABLE_QUERY_SERVICE=true \
  ENABLE_LEDGER_SERVICE=true \
  abci_validator_node >>/root/log 2>&1 &

nohup tendermint node >> /root/td.log 2>&1 &
