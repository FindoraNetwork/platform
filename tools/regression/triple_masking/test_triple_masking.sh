#!/usr/bin/env bash

set -e

EVM_SCRIPTS_PATH="tools/regression/evm/scripts"
TRIPLE_MASKING_SCRIPTS_PATH="tools/regression/triple_masking/scripts"
source $EVM_SCRIPTS_PATH/env.sh
source $TRIPLE_MASKING_SCRIPTS_PATH/env.sh
let SLEEP_INTERVAL=($BLOCK_INTERVAL + 1)

#Setup environment
./$EVM_SCRIPTS_PATH/setup.sh

#Run Tests
echo -e "${YEL}Run test cases and verify results${NC}"


./$TRIPLE_MASKING_SCRIPTS_PATH/create_test_bars.sh
#Verify
python $REGRESSION_PATH/evm.py verify-balance --sec-key $BAR_SEC_KEY --amount 840000000
echo

./$TRIPLE_MASKING_SCRIPTS_PATH/setup_wallets.sh


echo "\n\n\n Bar To Abar Conversion"
echo "==============================================================================="
# convert bar to abar
sleep 1
fn convert-bar-to-abar --anon-keys ./$FILE_ANON_KEYS  --txo-sid 3

sleep 20

#Verify
python $REGRESSION_PATH/evm.py verify-balance --sec-key $BAR_SEC_KEY --amount 629990000
echo

randomiser1=$(tail -n 1 owned_randomizers)
echo "\n\n Owned Abars after Bar to Abar conversion"
sleep 20 #Do not remove/decrease
fn owned-abars -p zQa8j0mGYUXM6JxjWN_pqfOi1lvEyenJYq35OIJNN08= -r $randomiser1


echo "\n\n\n Anonymous Transfer from Sender1 to Receiver1"
echo "------------------------------------------------------------------------------"
fn anon-transfer --amount 189990000 --anon-keys ./anon-keys-temp.keys --to-axfr-public-key ptyo7hlqn-Eywf_ttG99OSWtz6KfDUvLYDDVDlV1C-U= --to-enc-key SAmB7Oji4sAgENLaLb4PFclxQL_DRrEkXcYp6eXuXwI= --randomizer $randomiser1

randomiser2=$(tail -n 1 sent_randomizers)
echo "\n\n Owned Abars for Receiver1 after Anon Transfer 1"
sleep 30
echo $randomiser2 > /dev/null
fn owned-abars -p ptyo7hlqn-Eywf_ttG99OSWtz6KfDUvLYDDVDlV1C-U= -r $randomiser2

echo "\n\n\n Anonymous Transfer from Receiver1 (Sender2) to Receiver2"
echo "------------------------------------------------------------------------------"
fn anon-transfer --amount 169990000 --anon-keys ./anon-keys-temp2.keys --to-axfr-public-key BdECoTzLNQHlKq1oGMI2kdh27yp_I2CZen0FGYLFkM0= --to-enc-key Ox5L-mGxzOFfd4fef7WZGJMdO-EKBVnnJypZiEl_9FQ= --randomizer $randomiser2

randomiser3=$(tail -n 1 sent_randomizers)
echo "\n\n Owned Abars for Receiver2 after Anon Transfer 2"
sleep 30
echo $randomiser3 > /dev/null
fn owned-abars -p BdECoTzLNQHlKq1oGMI2kdh27yp_I2CZen0FGYLFkM0= -r $randomiser3

sleep 2
echo "\n\n\n Fetch merkle proof for Anon Transfer 2"
echo "------------------------------------------------------------------------------"
fn anon-fetch-merkle-proof -a 2

#Verify
python $REGRESSION_PATH/evm.py verify-balance --sec-key $BAR_SEC_KEY --amount 629990000
echo

fn owned-utxos

echo "\n\n\n Bar To Abar Conversion"
echo "==============================================================================="
# convert bar to abar
sleep 1
fn convert-bar-to-abar --anon-keys ./$FILE_ANON_KEYS  --txo-sid 9

sleep 20

#Verify
python $REGRESSION_PATH/evm.py verify-balance --sec-key $BAR_SEC_KEY --amount 419980000
echo

randomiser=$(tail -n 1 owned_randomizers)
echo "\n\n Owned Abars "
sleep 20 #Do not remove/decrease
target/debug/fn owned-abars -p zQa8j0mGYUXM6JxjWN_pqfOi1lvEyenJYq35OIJNN08= -r $randomiser

target/debug/fn convert-abar-to-bar --anon-keys ./anon-keys-temp.keys -r $randomiser --to-wallet-address  fra1ck6mu4fgmh7n3g0y5jm0zjrq6hwgckut9q2tf5fpwhrdgkhgdp9qhla5t5

#Verify
python $REGRESSION_PATH/evm.py verify-balance --sec-key $BAR_SEC_KEY --amount 419980000
echo