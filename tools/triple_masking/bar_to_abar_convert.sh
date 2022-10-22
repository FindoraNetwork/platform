source ./tools/devnet/env.sh || exit 1

echo "\n\n\n Simple transfer 1"
echo "------------------------------------------------------------------------------"
"$BIN"/fn transfer --amount 210000000 --asset FRA -T eth1q0gdeyyac8cmu7mwfuq6sgfy63dsvja6t877jwz44lyqjra76uh5sj2faa6
sleep 5

echo "\n\n\n Simple transfer 2"
echo "------------------------------------------------------------------------------"
"$BIN"/fn transfer --amount 210000000 --asset FRA -T eth1q0gdeyyac8cmu7mwfuq6sgfy63dsvja6t877jwz44lyqjra76uh5sj2faa6
sleep 5

echo "\n\n\n Simple transfer 3"
echo "------------------------------------------------------------------------------"
"$BIN"/fn transfer --amount 210000000 --asset FRA -T eth1q0gdeyyac8cmu7mwfuq6sgfy63dsvja6t877jwz44lyqjra76uh5sj2faa6
sleep 5

# setup the new wallet

FILE_MNEMONIC="mnemonic-temp.keys"
FILE_ANON_KEYS="anon-keys-temp.keys"

echo "double quit tape enough charge fancy mandate ostrich this program laundry insect either escape cement van turtle loud immense load tip spike inquiry spice" > $FILE_MNEMONIC

echo "
{
  \"spend_key\": \"6kpJDnAoL-_ZHekWoJBfrCmHnpYRs7WPMxdG_F9hJoQMhcLuDK2su2b4-IdYATM0Ou99yAPYcvSNdLSGnf5hBIA=\",
  \"pub_key\": \"DIXC7gytrLtm-PiHWAEzNDrvfcgD2HL0jXS0hp3-YQSA\"
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
