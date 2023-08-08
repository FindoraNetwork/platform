#!/usr/bin/env sh


set -e

ETH_ACCOUNT="eth1qyp46jrvem5crlhcgruluh9v4lk6ldxr7l92apd0hrxnudtjgfrgx5s7elq6s"
ETH_ACCOUNT_MNEMONIC_FILE="mnemonic-temp.keys"
ANON_1_KEYS_FILE="anon-keys-temp.keys"

set +e
rm $ETH_ACCOUNT_MNEMONIC_FILE $ANON_1_KEYS_FILE
set -e
echo "armed bean noodle wheat sing concert bench novel burden apology cliff yard soldier lobster lawn pink antique ticket ignore visual grief orchard grab dream" >> $ETH_ACCOUNT_MNEMONIC_FILE

echo "\n\n\n Simple transfer 1"
echo "------------------------------------------------------------------------------"
target/release/fn transfer --amount 210000000 --asset FRA -T $ETH_ACCOUNT
sleep 5

echo "\n\n\n Simple transfer 2"
echo "------------------------------------------------------------------------------"
target/release/fn transfer --amount 210000000 --asset FRA -T $ETH_ACCOUNT
sleep 5

echo "\n\n\n Simple transfer 3"
echo "------------------------------------------------------------------------------"
target/release/fn transfer --amount 210000000 --asset FRA -T $ETH_ACCOUNT
sleep 5

# setup the new wallet
echo "{
  \"xfr_secret_key\": \"Af7Rrv734hRVj_aMmHvWgGiyNDxQ-00m2mSPIpFjzbz-\",
  \"xfr_address\": \"eth1qypg2j03ww69ywq3y8wxvae35uqtxxfryy2ke6vt0x30xqclv56clugrrc9s2\"
}" >> $ANON_1_KEYS_FILE

echo "\n\n\n Bar To Abar Conversion"
echo "==============================================================================="
target/release/fn setup -O $ETH_ACCOUNT_MNEMONIC_FILE -S http://0.0.0.0
# convert bar to abar
sleep 10


ANON_1_ADDRESS=$(cat $ANON_1_KEYS_FILE | jq -r '.xfr_address')
target/release/fn convert-bar-to-abar --to-address $ANON_1_ADDRESS  --txo-sid 3 --eth-address

COMMITMENT_STRING=$(tail -n 1 owned_commitments)
echo $COMMITMENT_STRING
cat $ANON_1_KEYS_FILE | jq -r '.xfr_secret_key' > /tmp/tmp-sec-key
echo "Bar 2 Abar Conversion demo script executed successfully!"

sleep 10
target/release/fn owned-abars -s /tmp/tmp-sec-key  -c $COMMITMENT_STRING