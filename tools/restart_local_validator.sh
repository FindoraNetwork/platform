#!/bin/bash

set -e
ENV=$1

if [[ ${ENV} == "qa01" ]]; then
  export ROOTDIR=${ROOTDIR:=/tmp/qa01-node}
  NET=qa01-net
elif [[ ${ENV} == "testnet" ]]; then
  export ROOTDIR=${ROOTDIR:=/tmp/testnet-node}
  NET=test-net
else
  echo "only qa01 or testnet supported!"
  exit 0
fi

pkill -9 abcid || echo "no abcid found"

abcid node \
  --base-dir ${ROOTDIR}/tendermint \
  --config ${ROOTDIR}/tendermint/config/config.toml \
  --ledger-dir ${ROOTDIR}/findora \
  --tendermint-host 0.0.0.0 \
  --enable-ledger-service \
  --enable-query-service \
  --tendermint-node-key-config-path ${ROOTDIR}/tendermint/config/priv_validator_key.json \
  >> ${ROOTDIR}/abcid.log 2>&1 &