#!/usr/bin/env bash

set -e

fn setup -S http://127.0.0.1 2>&1 >/dev/null
V1_ADDR=$(curl -s http://127.0.0.1:8668/validator_list|jq -r '.validators[1].addr')
#ROOTSK=$(stt show -r|tail -1|awk -F "\"" '{print $8}')
#ROOTPK=$(stt show -r|tail -1|awk -F "\"" '{print $4}')


BANK_ADDR="fra18xkez3fum44jq0zhvwq380rfme7u624cccn3z56fjeex6uuhpq6qv9e4g5"
#Mnemonic: field ranch pencil chest effort coyote april move injury illegal forest amount bid sound mixture use second pet embrace twice total essay valve loan
#Key: {
BANK_PK="Oa2RRTzdayA8V2OBE7xp3n3NKrjGJxFTSZZybXOXCDQ="
BANK_SK="Ew9fMaryTL44ZXnEhcF7hQ-AB-fxgaC8vyCH-hCGtzg="
#}
echo "Bank account"
echo "   Bank Addr: ${BANK_ADDR}"
echo "   BANK_PK: ${BANK_PK}"
echo "   BANK_SK: ${BANK_SK}"
echo ""

# wallet operations
WALLET=$(fn wallet --create 2>&1)
USERADDR=$(echo $WALLET|grep "Wallet"|awk '{print $3}')
USERPK=$(echo $WALLET|grep "pub_key"|awk -F "\"" '{print $4}')
USERSK=$(echo $WALLET|grep "sec_key"|awk -F "\"" '{print $8}')

WALLET=$(fn wallet --create 2>&1)
USERADDR_1=$(echo $WALLET|grep "Wallet"|awk '{print $3}')
USERPK_1=$(echo $WALLET|grep "pub_key"|awk -F "\"" '{print $4}')
USERSK_1=$(echo $WALLET|grep "sec_key"|awk -F "\"" '{print $8}')

echo "Generating a new fra account"
echo "   FraAddr: ${USERADDR}"
echo "   publicKey: ${USERPK}"
echo "   SecretKey: ${USERSK}"

echo -n "${BANK_SK}" > banksk
echo -n "${USERSK}" > usersk
echo -n "${USERSK_1}" > usersk_1

echo "FRA balance"
fn wallet --show --seckey usersk 2>&1 | head -1
fn transfer -n $((1000*1000*1000*1000*1000)) --asset "fRa" -f banksk -T "${USERADDR}" 2>&1 > /dev/null
sleep 20
BALANCE=$(fn wallet --show --seckey usersk 2>&1 | head -1)
echo "${BALANCE}"
BALANCE=$(echo "${BALANCE}" | awk '{print $NF}')
if [ "${BALANCE}" -ne $((1000*1000*1000*1000*1000)) ]; then echo "Incorrect balance"; exit 1; fi

echo "Asset operations"
fn asset --create --seckey usersk --memo "asset0" --transferable 2>&1 >/dev/null
sleep 20
CODE=$(fn asset --show --addr "${USERADDR}" 2>&1 |head -1)
fn asset --issue --seckey usersk --code "${CODE}" --amount $((1000*1000*1000*1000*1000)) 2>&1 >/dev/null
sleep 20
BALANCE=$(fn wallet --show --seckey usersk --asset "${CODE}" 2>&1 |head -1 | awk '{print $NF}')
if [ "${BALANCE}" -ne $((1000*1000*1000*1000*1000)) ]; then echo "Incorrect custom balance"; exit 2; fi

echo "Transfer custom asset"
fn transfer -f usersk -T "${USERADDR_1}" --asset "${CODE}" -n $((1000*1000*1000*1000)) 2>&1 > /dev/null
sleep 20
BALANCE=$(fn wallet --show --seckey usersk_1 --asset "${CODE}" 2>&1 |head -1 | awk '{print $NF}')
if [ "${BALANCE}" -ne $((1000*1000*1000*1000)) ]; then echo "Incorrect custom balance"; exit 2; fi
BALANCE=$(fn wallet --show --seckey usersk --asset "${CODE}" 2>&1 |head -1 | awk '{print $NF}')
if [ "${BALANCE}" -ne $((1000*1000*1000*1000*999)) ]; then echo "Incorrect custom balance"; exit 2; fi

echo "Delegation operations"
fn delegate --seckey usersk --amount $((1000*1000*1000*1000*999)) --validator "${V1_ADDR}" 2>&1 >/dev/null
sleep 20
BOND=$(fn delegate --info --seckey usersk 2>&1 |grep -w "bond")
BOND=$(echo "${BOND}" | awk '{print $NF}' | awk -F "," '{print $1}')
echo "   bond: ${BOND}"
if [ "${BOND}" -ne $((1000*1000*1000*1000*999)) ]; then echo "Incorrect bond amount"; exit 3; fi
sleep 60
fn undelegate --seckey usersk --amount $((1000*1000*1000*1000)) --validator "${V1_ADDR}" 2>&1 > /dev/null
sleep 60
BOND=$(fn delegate --info --seckey usersk 2>&1 |grep -w "bond")
BOND=$(echo "${BOND}" | awk '{print $NF}' | awk -F "," '{print $1}')
echo "   bond: ${BOND}"
if [ "${BOND}" -ne $((1000*1000*1000*1000*998)) ]; then echo "Incorrect bond amount"; exit 3; fi
fn undelegate --seckey usersk 2>&1 > /dev/null
sleep 60
BOND=$(fn delegate --info --seckey usersk 2>&1 |grep -w "bond")
BOND=$(echo "${BOND}" | awk '{print $NF}' | awk -F "," '{print $1}')
echo "   bond: ${BOND}"
if [ "${BOND}" -ne 0 ]; then echo "Incorrect bond amount"; exit 3; fi

echo "Claim operations"
sleep 20
REWARDS=$(fn delegate --info --seckey usersk | grep -w "rewards" | awk '{print $NF}' | awk -F "," '{print $1}')
echo "   rewards: ${REWARDS}"
fn claim --amount 1000 --seckey usersk 2>&1 >/dev/null
sleep 20
REWARDS_1=$(fn delegate --info --seckey usersk | grep -w "rewards" | awk '{print $NF}' | awk -F "," '{print $1}')
echo "   rewards: ${REWARDS_1}"
if [ "${REWARDS_1}" -ne $((REWARDS-1000)) ]; then echo "Incorrect rewards amount"; exit 4; fi
fn claim --seckey usersk 2>&1 > /dev/null
sleep 20
REWARDS=$(fn delegate --info --seckey usersk | grep -w "rewards" | awk '{print $NF}' | awk -F "," '{print $1}')
echo "   rewards: ${REWARDS}"
if [ "${REWARDS}" -ne 0 ]; then echo "Incorrect rewards amount"; exit 5; fi

rm -f banksk usersk usersk_1
