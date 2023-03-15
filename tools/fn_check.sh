#! /bin/bash
set -e

echo "Set Faucet Mnemonic"
echo "zoo nerve assault talk depend approve mercy surge bicycle ridge dismiss satoshi boring opera next fat cinnamon valley office actor above spray alcohol giant" > /tmp/Faucet_Mnemonic
fn setup -O /tmp/Faucet_Mnemonic -S http://localhost

echo "Init network"
stt init
sleep 15

echo "1. transfer"
fn transfer -n 1000000 -t WJZKo8_BwLMeSz3h7jYdfjjBZlJiRdhUXkqgUVzReE0=

echo "2. confidential-amount transfer"
fn transfer -n 1000000 -t WJZKo8_BwLMeSz3h7jYdfjjBZlJiRdhUXkqgUVzReE0= --confidential-amount

echo "3. confidential-type transfer"
fn transfer -n 1000000 -t WJZKo8_BwLMeSz3h7jYdfjjBZlJiRdhUXkqgUVzReE0= --confidential-type

echo "4. confidential-amount and confidential-type transfer"
fn transfer -n 1000000 -t WJZKo8_BwLMeSz3h7jYdfjjBZlJiRdhUXkqgUVzReE0= --confidential-amount --confidential-type

echo "5. transfer-batch"
echo -e "WJZKo8_BwLMeSz3h7jYdfjjBZlJiRdhUXkqgUVzReE0=\nevNR_3Jk8lSMSSHNdkk3d8Lq2peHicpKl0-_Wf2clfo=\nEGrwne7CoB6c3NgydG_CBmQFBL-27Shd6_cZ68ktOR8=" > /tmp/batch_list        
fn transfer-batch -n 1000000 -t /tmp/batch_list

echo "6. Create Asset"
fn asset --create --memo TTT --decimal 6 --code VG9rZW4wMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDBUVFQ= --transferable
sleep 30

echo "7. Issue Asset"
fn asset --issue --code VG9rZW4wMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDBUVFQ= --amount 8000000
sleep 30

echo "8. Asset transfer"
fn transfer -n 1000000 -t WJZKo8_BwLMeSz3h7jYdfjjBZlJiRdhUXkqgUVzReE0= --asset VG9rZW4wMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDBUVFQ=
fn transfer -n 1000000 -t WJZKo8_BwLMeSz3h7jYdfjjBZlJiRdhUXkqgUVzReE0= --confidential-amount --asset VG9rZW4wMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDBUVFQ=
fn transfer -n 1000000 -t WJZKo8_BwLMeSz3h7jYdfjjBZlJiRdhUXkqgUVzReE0= --confidential-type --asset VG9rZW4wMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDBUVFQ=
fn transfer -n 1000000 -t WJZKo8_BwLMeSz3h7jYdfjjBZlJiRdhUXkqgUVzReE0= --confidential-amount --confidential-type --asset VG9rZW4wMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDBUVFQ=

echo "9. Stake"
fn delegate -n 1000000 --validator 611C922247C3BE7EA13455B191B6EFD909F10196
sleep 30

echo "10. Claim rewards"
fn claim

echo "11. Unstake"
fn undelegate -n 900000 --validator 611C922247C3BE7EA13455B191B6EFD909F10196

echo "12. UTXO to EVM"
fn contract-deposit -a 0xE89441fDD60b473fBaC19F6DBcee7e7D61Bd99e6 -n 1000000
sleep 30

echo "13. EVM to UTXO"
fn contract-withdraw -n 900000 -e "abuse plug bench require oval youth spike country ten pudding power hedgehog"
