set -e
./tools/triple_masking/bar_to_abar_convert.sh

COMMITMENT_STRING=$(tail -n 1 owned_commitments)
echo $COMMITMENT_STRING
cat anon-keys-temp.keys | jq -r '.xfr_secret_key' > /tmp/tmp-sec-key
echo "Bar 2 Abar Conversion demo script executed successfully!"

sleep 5
target/release/fn owned-abars -s /tmp/tmp-sec-key  -c $COMMITMENT_STRING

set +e
rm anon-keys-temp2.keys
echo "
{
  \"xfr_secret_key\": \"5jFpHsFeKQ8yb8SPn4daWgme4EWQaUBnlrnwL3uLV4g=\",
  \"xfr_address\": \"fra1waenjdxy2g89y60k02xna9uhkp67qzm5ce2nu5kg4ju94elzqrfsqw7pdh\"
}" >> anon-keys-temp2.keys
sleep 5
set -e

echo "\n\n\n Anonymous Transfer from Sender1 to Receiver1"
echo "------------------------------------------------------------------------------"
target/release/fn anon-transfer --amount 110000000 --from-seckey /tmp/tmp-sec-key --to-address fra1waenjdxy2g89y60k02xna9uhkp67qzm5ce2nu5kg4ju94elzqrfsqw7pdh  --commitment $COMMITMENT_STRING

cat anon-keys-temp2.keys | jq -r '.xfr_secret_key' > /tmp/tmp-sec-key
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
