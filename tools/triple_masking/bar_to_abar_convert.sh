source ./tools/devnet/env.sh || exit 1

echo "\n\n\n Simple transfer 1"
echo "------------------------------------------------------------------------------"
"$BIN"/fn transfer --amount 210000000 --asset FRA -T fra1ck6mu4fgmh7n3g0y5jm0zjrq6hwgckut9q2tf5fpwhrdgkhgdp9qhla5t5
sleep 5

echo "\n\n\n Simple transfer 2"
echo "------------------------------------------------------------------------------"
"$BIN"/fn transfer --amount 210000000 --asset FRA -T fra1ck6mu4fgmh7n3g0y5jm0zjrq6hwgckut9q2tf5fpwhrdgkhgdp9qhla5t5
sleep 5

echo "\n\n\n Simple transfer 3"
echo "------------------------------------------------------------------------------"
"$BIN"/fn transfer --amount 210000000 --asset FRA -T fra1ck6mu4fgmh7n3g0y5jm0zjrq6hwgckut9q2tf5fpwhrdgkhgdp9qhla5t5
sleep 5

# setup the new wallet

FILE_MNEMONIC="mnemonic-temp.keys"
FILE_ANON_KEYS="anon-keys-temp.keys"

echo "double quit tape enough charge fancy mandate ostrich this program laundry insect either escape cement van turtle loud immense load tip spike inquiry spice" > $FILE_MNEMONIC

echo "
{
  \"spend_key\": \"Ccv2h8u1g__HJBrsA8npcs4CiDQ_UHI-JGZCjXbu9Un8HU3qSTf3PdLEFvs1XwauSltgruFv-IRVFpaQkeIIAgRoRPXncS1VHYzRpQlghzgCcQKJnic90DFDiYxSPVjg\",
  \"view_key\": \"_B1N6kk39z3SxBb7NV8GrkpbYK7hb_iEVRaWkJHiCAI=\",
  \"pub_key\": \"BGhE9edxLVUdjNGlCWCHOAJxAomeJz3QMUOJjFI9WOA=\"
}" > $FILE_ANON_KEYS

"$BIN"/fn setup -O $FILE_MNEMONIC -S http://0.0.0.0

set -e
"$BIN"/fn owned-utxos
echo "BAR Balance:"
"$BIN"/fn wallet --show
# convert bar to abar
echo "\n\n\n Bar To Abar Conversion 1"
echo "==============================================================================="
sleep 1
TXO_SID=$("$BIN"/fn owned-utxos | head -4 | tail -1 |  awk -F ' ' '{print $1}')
"$BIN"/fn convert-bar-to-abar --anon-keys ./$FILE_ANON_KEYS  --txo-sid $TXO_SID

echo "\n\n\n Bar To Abar Conversion 2"
echo "==============================================================================="
sleep 5
TXO_SID=$("$BIN"/fn owned-utxos | head -4 | tail -1 | awk -F ' ' '{print $1}')
"$BIN"/fn convert-bar-to-abar --anon-keys ./$FILE_ANON_KEYS  --txo-sid $TXO_SID

echo "Bar 2 Abar Conversion demo script executed successfully!"
echo "To check generated Abars run \`"$BIN"/fn owned-abars --anon-keys ./$FILE_ANON_KEYS --commitments COMMITMENT_STRING \`"
