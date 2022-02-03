#!/usr/bin/env bash
source tools/regression/evm/scripts/env.sh

# ------------------------ Teardown ------------------------
echo -e "${YEL}Stop network and remove test data${NC}"
./$DEVNET_TOOLS_PATH/stopnodes.sh
rm -rf $DEVNET/*