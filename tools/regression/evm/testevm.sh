#!/usr/bin/env bash

EVM_SCRIPTS_PATH="tools/regression/evm/scripts"
source $EVM_SCRIPTS_PATH/env.sh
let SLEEP_INTERVAL=($BLOCK_INTERVAL + 1)


#Run Tests
echo -e "${YEL}Run test cases and verify results${NC}"

#--------------------------test case 1 - contract deposit--------------------------
echo -e "${BLU}test case 1 - contract deposit${NC}"
$BIN/fn contract-deposit --addr $ETH_ADDR --amount 888000000
sleep $SLEEP_INTERVAL
#Verify
python3 $REGRESSION_PATH/evm.py --url $ENDPOINT verify-balance --addr $ETH_ADDR --amount 888000000000000000000
if [ $? != 0 ];
then
    exit 1
fi
echo

#--------------------------test case 2 - contract withdraw---------------------------
echo -e "${BLU}test case 2 - contract withdraw${NC}"
$BIN/fn contract-withdraw --addr $FRA_DEST_ADDR --amount 88000000 --eth-key "$ETH_KEY"
sleep $SLEEP_INTERVAL
#Verify
python3 $REGRESSION_PATH/evm.py --url $ENDPOINT verify-balance --sec-key $FRA_SEC_KEY --amount 88000000
if [ $? != 0 ];
then
    exit 1
fi
echo

#--------------------------test case 3 - ERC20 transfer-----------------------------
echo -e "${BLU}test case 3 - transfer between erc20 addresses${NC}"
python3 $REGRESSION_PATH/evm.py --url $ENDPOINT transfer --from_priv_key $ETH_PK --to_addr $ETH_DEST_ADDR --amount 8000000000000000000
sleep $SLEEP_INTERVAL
#Verify
python3 $REGRESSION_PATH/evm.py --url $ENDPOINT verify-balance --addr $ETH_DEST_ADDR --amount 8000000000000000000
if [ $? != 0 ];
then
    exit 1
fi
echo
