#!/usr/bin/env bash

EVM_SCRIPTS_PATH="tools/regression/evm/scripts"
source $EVM_SCRIPTS_PATH/env.sh
let SLEEP_INTERVAL=($BLOCK_INTERVAL + 1)

if [ ! -z "$ENV_FRA_DEST_ADDR" ]
then
    FRA_DEST_ADDR="$ENV_FRA_DEST_ADDR"
fi
if [ ! -z "$ENV_FRA_SEC_KEY" ]
then
    FRA_SEC_KEY="$ENV_FRA_SEC_KEY"
fi
if [ ! -z "$ENV_ETH_KEY" ]
then
    ETH_KEY="$ENV_ETH_KEY"
fi
if [ ! -z "$ENV_ETH_PK" ]
then
    ETH_PK="$ENV_ETH_PK"
fi
if [ ! -z "$ENV_ETH_ADDR" ]
then
    ETH_ADDR="$ENV_ETH_ADDR"
fi
if [ ! -z "$ENV_ETH_DEST_ADDR" ]
then
    ETH_DEST_ADDR="$ENV_ETH_DEST_ADDR"
fi

#Run Tests
echo -e "${YEL}Run test cases and verify results${NC}"

#--------------------------test case 1 - contract deposit--------------------------
echo -e "${BLU}test case 1 - contract deposit${NC}"
#Get Initial Balance of $ETH_ADDR
export OUTPUT=$(python3 $REGRESSION_PATH/evm.py --url $ENDPOINT balance --addr $ETH_ADDR)
BALANCE=`echo $OUTPUT |awk '{print $3}'`
#Deposit amount into Eth Address
$BIN/fn contract-deposit --addr $ETH_ADDR --amount 888000000
sleep $SLEEP_INTERVAL
EXPECTED_BALANCE="$( bc <<<" $BALANCE + 888000000000000000000" )"
#Verify
python3 $REGRESSION_PATH/evm.py --url $ENDPOINT verify-balance --addr $ETH_ADDR --amount $EXPECTED_BALANCE
if [ $? != 0 ];
then
    exit 1
fi
echo

#--------------------------test case 2 - contract withdraw---------------------------
echo -e "${BLU}test case 2 - contract withdraw${NC}"
#Get Initial Balance of $ETH_ADDR
export OUTPUT=$(python3 $REGRESSION_PATH/evm.py --url $ENDPOINT balance --sec-key $FRA_SEC_KEY)
BALANCE=`echo $OUTPUT |awk '{print $3}'`
#Withdraw amount from Eth Address
$BIN/fn contract-withdraw --addr $FRA_DEST_ADDR --amount 88000000 --eth-key "$ETH_KEY"
sleep $SLEEP_INTERVAL
EXPECTED_BALANCE="$( bc <<<" $BALANCE + 88000000 " )"
#Verify
python3 $REGRESSION_PATH/evm.py --url $ENDPOINT verify-balance --sec-key $FRA_SEC_KEY --amount $EXPECTED_BALANCE
if [ $? != 0 ];
then
    exit 1
fi
echo

#--------------------------test case 3 - ERC20 transfer-----------------------------
echo -e "${BLU}test case 3 - transfer between erc20 addresses${NC}"
#Get Initial Balance of $ETH_ADDR
export OUTPUT=$(python3 $REGRESSION_PATH/evm.py --url $ENDPOINT balance --addr $ETH_DEST_ADDR)
BALANCE=`echo $OUTPUT |awk '{print $3}'`
#Transer between ETH Addresses
python3 $REGRESSION_PATH/evm.py --url $ENDPOINT transfer --from_priv_key $ETH_PK --to_addr $ETH_DEST_ADDR --amount 8000000000000000000
sleep $SLEEP_INTERVAL
EXPECTED_BALANCE="$( bc <<<" $BALANCE + 8000000000000000000 " )"
#Verify
python3 $REGRESSION_PATH/evm.py --url $ENDPOINT verify-balance --addr $ETH_DEST_ADDR --amount $EXPECTED_BALANCE
if [ $? != 0 ];
then
    exit 1
fi
echo
