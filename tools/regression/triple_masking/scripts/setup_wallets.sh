#rm mnemonic-temp.keys anon-keys-temp.keys
source tools/regression/triple_masking/scripts/env.sh
# These wallets would be used to move Anonymous funds around

echo "double quit tape enough charge fancy mandate ostrich this program laundry insect either escape cement van turtle loud immense load tip spike inquiry spice" > mnemonic-temp.keys

echo "
{
  \"spend_key\": \"Ccv2h8u1g__HJBrsA8npcs4CiDQ_UHI-JGZCjXbu9Un8HU3qSTf3PdLEFvs1XwauSltgruFv-IRVFpaQkeIIAgRoRPXncS1VHYzRpQlghzgCcQKJnic90DFDiYxSPVjg\",
  \"view_key\": \"_B1N6kk39z3SxBb7NV8GrkpbYK7hb_iEVRaWkJHiCAI=\",
  \"pub_key\": \"BGhE9edxLVUdjNGlCWCHOAJxAomeJz3QMUOJjFI9WOA=\"
}" > "$FILE_ANON_KEYS"

$BIN/fn setup -O mnemonic-temp.keys -S $ENDPOINT

echo "New BAR wallet with Balance:"
$BIN/fn wallet --show


rm anon-keys-temp2.keys
echo "
{
  \"spend_key\": \"h4MuWol8pWuNIMxPHwJ0ZAoF_n51QScj6AultG5IHU3yL-LR02XXw58uudwom_tahcy1e0oadfOw3oLxSs64A9yTOKFC1NqT6e-fWGEO-QpSZzf8otV7POguvdejoKhL\",
  \"view_key\": \"8i_i0dNl18OfLrncKJv7WoXMtXtKGnXzsN6C8UrOuAM=\",
  \"pub_key\": \"3JM4oULU2pPp759YYQ75ClJnN_yi1Xs86C6916OgqEs=\"
}" > "$FILE_ANON_KEYS_2"

echo "
 {
  \"spend_key\": \"bRrcmHV-87-na2jKuOEQZmVyLE6q4oVdCiMoWdqVHwOqkAlAXybyeheaNCyWw7j0lz4vlnxP5nUNpbnSwF3tBiXKJs7KF1X9zc9ZUy_3U8-2YnyrGSWbQ-QIpNVmBGvy\",
  \"view_key\": \"qpAJQF8m8noXmjQslsO49Jc-L5Z8T-Z1DaW50sBd7QY=\",
  \"pub_key\": \"JcomzsoXVf3Nz1lTL_dTz7ZifKsZJZtD5Aik1WYEa_I=\"
}
"  > "$FILE_ANON_KEYS_3"
sleep 1