#!/usr/bin/env bash

set -e

# run setup scripts
EVM_SCRIPTS_PATH="tools/regression/evm/scripts"
TRIPLE_MASKING_SCRIPTS_PATH="tools/regression/triple_masking/scripts"
source $EVM_SCRIPTS_PATH/env.sh
source $TRIPLE_MASKING_SCRIPTS_PATH/env.sh
# Setup environment
./$EVM_SCRIPTS_PATH/setup.sh

# declare sleep intervals
TM_SLEEP=25 # If breaking, increase the sleep time


# Run Tests
echo -e "${YEL}Run test cases and verifying results${NC}"


./$TRIPLE_MASKING_SCRIPTS_PATH/create_test_bars.sh
# Verify
python "$REGRESSION_PATH"/evm.py verify-balance --sec-key $BAR_SEC_KEY --amount 840000000
echo

./$TRIPLE_MASKING_SCRIPTS_PATH/setup_wallets.sh


echo -e "\n ***** Bar To Abar Conversion *****"
# convert bar to abar
TXO_SID=$(target/release/fn owned-utxos | head -4 | tail -1 |  awk -F ' ' '{print $1}')
sleep 1
target/release/fn convert-bar-to-abar --anon-keys ./$FILE_ANON_KEYS  --txo-sid $TXO_SID
sleep $TM_SLEEP

#Verify
echo "\n ***** Verifying balances ***** "
python $REGRESSION_PATH/evm.py verify-balance --sec-key $BAR_SEC_KEY --amount 629980000
echo
commitment1=$(tail -n 1 owned_commitments)
python $REGRESSION_PATH/evm.py verify-anon-balance --anon-keys ./$FILE_ANON_KEYS --commitments $commitment1 --amount 210000000


echo -e "\n ***** Anonymous Transfer from Sender1 to Receiver1 ***** "
target/release/fn anon-transfer --amount 189990000 --anon-keys ./$FILE_ANON_KEYS --to-axfr-public-key ptyo7hlqn-Eywf_ttG99OSWtz6KfDUvLYDDVDlV1C-U= --to-enc-key SAmB7Oji4sAgENLaLb4PFclxQL_DRrEkXcYp6eXuXwI= --commitment $commitment1 > /dev/null
sleep $TM_SLEEP

commitment2=$(tail -n 1 sent_commitments)
echo -e "\n ***** Owned Abars for Receiver1 after Anon Transfer 1 ***** "
target/release/fn owned-abars --commitments $commitment2 --anon-keys ./$FILE_ANON_KEYS_2

#Verify
echo -e "\n ***** Verifying balances ***** "
python "$REGRESSION_PATH"/evm.py verify-anon-balance --anon-keys ./$FILE_ANON_KEYS --commitments $commitment1 --amount 0
python "$REGRESSION_PATH"/evm.py verify-anon-balance --anon-keys ./$FILE_ANON_KEYS_2 --commitments $commitment2 --amount 189990000


echo -e "\n ***** Anonymous Transfer from Receiver1 (Sender2) to Receiver2 ***** "
target/release/fn anon-transfer --amount 169990000 --anon-keys ./$FILE_ANON_KEYS_2 --to-axfr-public-key BlF_6qIKPpV1L4Z16NYGLj_GLKCSa6O5DZaF8fMHw0g= --to-enc-key NBY5yIhdriJVq-7BS59J2IxBgLhewr8TEE6suNc1elA= --commitment $commitment2 > /dev/null
sleep $TM_SLEEP

commitment3=$(tail -n 1 sent_commitments)
echo -e "\n ***** Owned Abars for Receiver2 after Anon Transfer 2 ***** "
target/release/fn owned-abars --commitments $commitment3 --anon-keys ./$FILE_ANON_KEYS_3

#Verify
echo -e "\n ***** Verifying balances ***** "
python $REGRESSION_PATH/evm.py verify-anon-balance --anon-keys ./$FILE_ANON_KEYS_2 --commitments $commitment2 --amount 0
python $REGRESSION_PATH/evm.py verify-anon-balance --anon-keys ./$FILE_ANON_KEYS_3 --commitments $commitment3 --amount 169990000

sleep 2
echo -e "\n Fetch merkle proof for Anon Transfer 2"
target/release/fn anon-fetch-merkle-proof -a 2 > /dev/null

#Verify
python "$REGRESSION_PATH"/evm.py verify-balance --sec-key "$BAR_SEC_KEY" --amount 629980000
echo

target/release/fn owned-utxos

echo -e "\n ***** Bar To Abar Conversion ***** "
# convert bar to abar
sleep 1

TXO_SID=$(target/release/fn owned-utxos | sed \$d | sed \$d | sed \$d |sort -k 1 -n | head -4 | tail -1 |  awk -F ' ' '{print $1}')
target/release/fn convert-bar-to-abar --anon-keys ./"$FILE_ANON_KEYS"  --txo-sid "$TXO_SID"
sleep $TM_SLEEP

commitment4=$(tail -n 1 owned_commitments)
echo -e "\n Owned Abars after Bar to Abar conversion"
target/release/fn owned-abars --commitments "$commitment4" --anon-keys ./"$FILE_ANON_KEYS"


#Verify
echo -e "\n ***** Verifying balances ***** "
python "$REGRESSION_PATH"/evm.py verify-balance --sec-key "$BAR_SEC_KEY" --amount 419960000
python "$REGRESSION_PATH"/evm.py verify-anon-balance --anon-keys ./"$FILE_ANON_KEYS" --commitments "$commitment4" --amount 210000000


echo -e "\n ***** Abar To Bar Conversion ***** "
target/release/fn convert-abar-to-bar --anon-keys ./"$FILE_ANON_KEYS" -c "$commitment4" --to-wallet-address fra1ck6mu4fgmh7n3g0y5jm0zjrq6hwgckut9q2tf5fpwhrdgkhgdp9qhla5t5 > /dev/null
sleep $TM_SLEEP

#Verify
echo -e "\n ***** Verifying balances ***** "
python "$REGRESSION_PATH"/evm.py verify-balance --sec-key "$BAR_SEC_KEY" --amount 629960000
python "$REGRESSION_PATH"/evm.py verify-anon-balance --anon-keys ./"$FILE_ANON_KEYS" --commitments "$commitment4" --amount 0
