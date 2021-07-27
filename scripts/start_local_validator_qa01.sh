#!/bin/bash

set -e
rm -rf /root/findora /root/.tendermint
mkdir /root/findora

tendermint init

toml unset --toml-path /root/.tendermint/config/config.toml tx_index.index_keys
toml set   --toml-path /root/.tendermint/config/config.toml tx_index.index_all_keys true
toml set   --toml-path /root/.tendermint/config/config.toml rpc.laddr tcp://0.0.0.0:26657

toml set   --toml-path /root/.tendermint/config/config.toml consensus.create_empty_blocks_interval 15s
toml set   --toml-path /root/.tendermint/config/config.toml p2p.persistent_peers \
"b87304454c0a0a0c5ed6c483ac5adc487f3b21f6@dev-qa01-us-west-2-sentry-000-public.dev.findora.org:26656,\
d0c6e3e1589695ae6d650b288caf2efe9a998a50@dev-qa01-us-west-2-sentry-001-public.dev.findora.org:26656"

curl https://dev-qa01.dev.findora.org:26657/genesis \
  | jq -c '.result.genesis' \
  | jq > /root/.tendermint/config/genesis.json

TD_NODE_SELF_ADDR=$(jq --raw-output '.address' /root/.tendermint/config/priv_validator_key.json) \
  LEDGER_DIR=/root/findora \
  ENABLE_QUERY_SERVICE=true \
  ENABLE_LEDGER_SERVICE=true \
  abci_validator_node >>/root/log 2>&1 &

nohup tendermint node >> /root/td.log 2>&1 &
