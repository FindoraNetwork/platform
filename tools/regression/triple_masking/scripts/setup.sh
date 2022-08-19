#!/usr/bin/env bash
source tools/regression/triple_masking/scripts/env.sh

echo -e "${YEL}Setup environment for triple masking testing${NC}"

let SLEEP_INTERVAL=($BLOCK_INTERVAL + 2)

# Start Nodes and initialize wallet
./$DEVNET_TOOLS_PATH/resetnodes.sh
sleep $SLEEP_INTERVAL
./$DEVNET_TOOLS_PATH/cleannodes.sh
./$DEVNET_TOOLS_PATH/startnodes.sh
sleep $SLEEP_INTERVAL
echo Regression path: $TM_REGRESSION_PATH
$BIN/stt init -i $BLOCK_INTERVAL -s
sleep $SLEEP_INTERVAL