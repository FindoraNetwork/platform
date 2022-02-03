#!/usr/bin/env bash

EVM_SCRIPTS_PATH="tools/regression/evm/scripts"
source $EVM_SCRIPTS_PATH/env.sh

#Setup environment
./$EVM_SCRIPTS_PATH/setup.sh

#Run Tests
echo -e "${YEL}Run test cases and verify results${NC}"

#--------------------------test case 1 - contract deposit--------------------------
echo -e "${BLU}test case 1 - contract deposit${NC}"
fn contract-deposit --addr $ETH_ADDR --amount 888000000
sleep 6
#Verify
python $REGRESSION_PATH/evm.py verify-balance --addr $ETH_ADDR --amount 888000000000000000000
echo
#--------------------------test case 2 - contract withdraw---------------------------
echo -e "${BLU}test case 2 - contract withdraw${NC}"
fn contract-withdraw --addr $FRA_DEST_ADDR --amount 88000000 --eth-key "$ETH_KEY"
sleep 6
#Verify
python $REGRESSION_PATH/evm.py verify-balance --sec-key $FRA_SEC_KEY --amount 88000000
echo

#echo -e "${GRN}test case 3 - transfer between eth addresses${NC}"

#Clean up environment
./$EVM_SCRIPTS_PATH/teardown.sh
