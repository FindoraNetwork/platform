set -e
# setup init bars for testing
./tools/triple_masking/bar_to_abar_convert.sh

FRA_ACCOUNT="fra1ck6mu4fgmh7n3g0y5jm0zjrq6hwgckut9q2tf5fpwhrdgkhgdp9qhla5t5"
FRA_ACCOUNT_MNEMONIC_FILE="mnemonic-temp.keys"

ANON_1_KEYS_FILE="anon-keys-temp.keys"
ANON_2_KEYS_FILE="anon-keys-temp2.keys"

COMMITMENT_STRING=$(tail -n 1 owned_commitments)
echo $COMMITMENT_STRING
cat $ANON_1_KEYS_FILE | jq -r '.xfr_secret_key' > /tmp/tmp-sec-key
echo "Bar 2 Abar Conversion demo script executed successfully!"

sleep 5
target/release/fn owned-abars -s /tmp/tmp-sec-key  -c $COMMITMENT_STRING

set +e
rm $ANON_2_KEYS_FILE
echo "
{
  \"xfr_secret_key\": \"5jFpHsFeKQ8yb8SPn4daWgme4EWQaUBnlrnwL3uLV4g=\",
  \"xfr_address\": \"fra1waenjdxy2g89y60k02xna9uhkp67qzm5ce2nu5kg4ju94elzqrfsqw7pdh\"
}" >> $ANON_2_KEYS_FILE
sleep 5
set -e
ANON_2_ADDRESS=$(cat $ANON_2_KEYS_FILE | jq -r '.xfr_address')

echo "\n\n\n Anonymous Transfer from Sender1 to Receiver1"
echo "------------------------------------------------------------------------------"
target/release/fn anon-transfer --amount 110000000 --from-seckey /tmp/tmp-sec-key --to-address $ANON_2_ADDRESS  --commitment $COMMITMENT_STRING

cat $ANON_2_KEYS_FILE | jq -r '.xfr_secret_key' > /tmp/tmp-sec-key
commitment=$(tail -n 1 sent_commitments)
echo "\n\n Owned Abars for Receiver1 after Anon Transfer 1"
sleep 20
target/release/fn owned-abars --from-seckey /tmp/tmp-sec-key -c $commitment

echo "\n\n\n Anonymous Transfer from Receiver1 (Sender2) to Receiver2"
echo "------------------------------------------------------------------------------"
target/release/fn anon-transfer --amount 10000000 --from-seckey /tmp/tmp-sec-key --to-address fra1cf5frqcxu0n8htdpxy0tjf0mpx9p02fu9y9n4da9cvuahzgc308sr83el4  --commitment $commitment

echo "9m7TS5UkIT8hhpe63IP9Whw6Ap5ozFCzfs1MaEJyJs8=" > /tmp/tmp-sec-key
COMMITMENT_STRING=$(tail -n 1 sent_commitments)
echo $COMMITMENT_STRING
echo "\n\n Owned Abars for Receiver2 after Anon Transfer 2"
sleep 20
target/release/fn owned-abars -s /tmp/tmp-sec-key  -c $COMMITMENT_STRING

sleep 2
echo "\n\n\n Fetch merkle proof for Anon Transfer 2"
echo "------------------------------------------------------------------------------"
target/release/fn anon-fetch-merkle-proof -a 2

echo "\n\n Anonymous Transfer demo script executed successfully!"
