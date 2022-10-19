#!/usr/bin/env bash
source tools/regression/evm/scripts/env.sh

echo -e "${YEL}Setup environment for evm testing${NC}"

let SLEEP_INTERVAL=($BLOCK_INTERVAL + 2)

# Start Nodes and initialize wallet
./$DEVNET_TOOLS_PATH/resetnodes.sh
sleep $SLEEP_INTERVAL
./$DEVNET_TOOLS_PATH/cleannodes.sh
./$DEVNET_TOOLS_PATH/startnodes.sh
sleep $SLEEP_INTERVAL
echo Regression path: $REGRESSION_EVM_PATH
$BIN/stt init -i $BLOCK_INTERVAL -s
sleep $SLEEP_INTERVAL