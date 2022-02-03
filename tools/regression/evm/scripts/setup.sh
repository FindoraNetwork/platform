#!/usr/bin/env bash
source tools/regression/evm/scripts/env.sh

echo -e "${YEL}Setup environment for evm testing${NC}"

# Stop existing nodes and clear the data
./$DEVNET_TOOLS_PATH/stopnodes.sh
rm -rf $DEVNET/*
# Copy node data to working directory
cp -rf $REGRESSION_EVM_PATH/devnet/* $DEVNET

# Start Nodes and initialize wallet
./$DEVNET_TOOLS_PATH/startnodes.sh
$BIN/fn setup -S http://0.0.0.0 > /dev/null
$BIN/fn setup -O $REGRESSION_EVM_PATH/mnemonic.key > /dev/null
$BIN/stt init -i $BLOCK_INTERVAL -s