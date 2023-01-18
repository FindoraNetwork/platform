source ./tools/devnet/env.sh || exit 1

# If breaking because of blockchain, increase the sleep time
FILE_MNEMONIC_2="mnemonic-temp2.keys"
FILE_MNEMONIC_3="mnemonic-temp3.keys"

echo "neutral mirror deliver skirt rice art pulse luxury venue switch worry recycle report tackle broom beyond rapid before test supreme suggest educate cluster best" > "$FILE_MNEMONIC_2"
echo "mix sign soul link expose dinner follow bomb anchor super wisdom host baby rich west escape shock ready this primary sound rib predict round" > "$FILE_MNEMONIC_3"

set -e
./tools/triple_masking/bar_to_abar_convert.sh

commitment1=$(tail -n 1 owned_commitments)
echo "\n\n Owned Abars after Bar to Abar conversion 1"
sleep 30
"$BIN"/fn owned-abars --commitments "$commitment1"

echo "\n\n\n Anonymous Transfer from Sender1 to Receiver1"
echo "------------------------------------------------------------------------------"
"$BIN"/fn anon-transfer --amount 189990000 --to-address fra17g378zk4qn48999p8k0kdw7eawah5svayxp3xdsw4tjxkarq8suqudh6fr --commitment "$commitment1"

"$BIN"/fn setup -O $FILE_MNEMONIC_2 -S http://0.0.0.0

commitment2=$(tail -n 1 sent_commitments)
echo -e "\n\n Owned Abars for Receiver1 after Anon Transfer 1"
sleep 30
"$BIN"/fn owned-abars --commitments "$commitment2"

echo "\n\n\n Anonymous Transfer from Receiver1 (Sender2) to Receiver2"
echo "------------------------------------------------------------------------------"
"$BIN"/fn anon-transfer --amount 169990000 --to-address fra10n9kmllr86fxh3jaechayh9jprnq56lygchtpnnvaj68ha5cesnsa6549z --commitment "$commitment2"

"$BIN"/fn setup -O $FILE_MNEMONIC_3 -S http://0.0.0.0

sleep 2
echo "\n\n\n Fetch merkle proof for Anon Transfer 2"
echo "------------------------------------------------------------------------------"
"$BIN"/fn anon-fetch-merkle-proof -a 2
echo "\n\n Anonymous Transfer demo script executed successfully!"
