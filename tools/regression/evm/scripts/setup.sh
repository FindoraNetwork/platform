#!/usr/bin/env bash
source tools/regression/evm/scripts/env.sh

echo -e "${YEL}Setup environment for evm testing${NC}"

# Start Nodes and initialize wallet
./$DEVNET_TOOLS_PATH/resetnodes.sh
sleep 5
./$DEVNET_TOOLS_PATH/cleannodes.sh
./$DEVNET_TOOLS_PATH/startnodes.sh
sleep 3
$BIN/fn setup -S http://0.0.0.0 > /dev/null
echo Regression path: $REGRESSION_EVM_PATH
$BIN/fn setup -O $REGRESSION_EVM_PATH/mnemonic.key > /dev/null
$BIN/stt init -i $BLOCK_INTERVAL -s
sleep 10