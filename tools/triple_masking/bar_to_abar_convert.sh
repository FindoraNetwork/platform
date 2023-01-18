source ./tools/devnet/env.sh || exit 1

echo "\n\n\n Simple transfer 1"
echo "------------------------------------------------------------------------------"
"$BIN"/fn transfer --amount 210000000 --asset FRA -T fra1qn3jkzenrktm8fmw3fmcalwzzzjwmvdry0lxzvm9lw32e4df8x0q2k7cc6
sleep 5

echo "\n\n\n Simple transfer 2"
echo "------------------------------------------------------------------------------"
"$BIN"/fn transfer --amount 210000000 --asset FRA -T fra1qn3jkzenrktm8fmw3fmcalwzzzjwmvdry0lxzvm9lw32e4df8x0q2k7cc6
sleep 5

echo "\n\n\n Simple transfer 3"
echo "------------------------------------------------------------------------------"
"$BIN"/fn transfer --amount 210000000 --asset FRA -T fra1qn3jkzenrktm8fmw3fmcalwzzzjwmvdry0lxzvm9lw32e4df8x0q2k7cc6
sleep 5

# setup the new wallet

FILE_MNEMONIC="mnemonic-temp.keys"

echo "ivory meadow bag slide illegal phone pelican point twist eight devote view law forum loud miss host nerve mother exhaust chunk flag work arch" > $FILE_MNEMONIC

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
"$BIN"/fn convert-bar-to-abar --to-address fra1qn3jkzenrktm8fmw3fmcalwzzzjwmvdry0lxzvm9lw32e4df8x0q2k7cc6 --txo-sid $TXO_SID

echo "\n\n\n Bar To Abar Conversion 2"
echo "==============================================================================="
sleep 5
TXO_SID=$("$BIN"/fn owned-utxos | head -4 | tail -1 | awk -F ' ' '{print $1}')
"$BIN"/fn convert-bar-to-abar --to-address fra1qn3jkzenrktm8fmw3fmcalwzzzjwmvdry0lxzvm9lw32e4df8x0q2k7cc6 --txo-sid $TXO_SID

echo "Bar 2 Abar Conversion demo script executed successfully!"
echo "To check generated Abars run \`"$BIN"/fn owned-abars --commitments COMMITMENT_STRING \`"
