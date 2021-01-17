#!/usr/bin/env bash

# clean abci
echo "reset abci: $LEDGER_DIR"
mkdir -p $LEDGER_DIR
rm -f $LEDGER_DIR/*

# reset tendermint node
echo "reset node: $HOME/.tendermint"
rm -rf $HOME/.tendermint
tendermint init > /dev/null
toml set --toml-path $HOME/.tendermint/config/config.toml consensus.create_empty_blocks_interval 10s
toml set --toml-path $HOME/.tendermint/config/config.toml consensus.timeout_commit 15s
toml set --toml-path $HOME/.tendermint/config/config.toml tx_index.index_all_keys true
