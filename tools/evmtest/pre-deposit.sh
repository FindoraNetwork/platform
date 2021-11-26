#!/usr/bin/env bash

ADDRs=$(cat << EOF
0x292F65000a1F09cC0f2dcdE656674E7De1fBeEa7
0x20bEf686d82f567dfc6ECc803732Ef99D9A19FEb
0x4C0a8a149e92D368F14aFB286BC1e0ecEaa4AaFe
0x3e930Ef583c3E7940ABDA49d7086302Ce703A071
0x5b9B9a20f1820487b4247F001aBCfa6500C7Ae37
0x96521d1b8AEeB261E264e6E76eaCd11D170f2CdC
0x5a76E8623d95486DA16b40B36479571756D7Cd00
0xbD1F881C7a5f3177a4e3bBb86557C26A484389A2
0x272B7509592E601f26b3a97f6416Af287E042C4e
0xc48726998C5ec4DD00D13b2050CF02CAaF7726eb
0x30eEacFC5C2549C14F0974eD33223D257aCc6f14
0xc5De2952bE452034329c37c885eBE379377b0495
0xDd2D2742d205576c96a042D6033bf0C22bb47b9c
0xC5044899eF1D477d60ea7F294AA7344B1116e0fD
0xD3c3f99bC6148541B7E6dD533773fe59c5Fc7148
0x0be7769068838A48553bB08D79015c32eA733445
0xBe23b14F3f8470f59E959665d4Ec537316bf2054
0xD92341af5cEE8E956C3c6e2df93A87d0F4469E2D
0xa600049c69305E0A389E53ac57e0933863252865
0xd8513228409e4174562eFFf3B0AD6344d336177F
EOF
)

source ./common/common.sh

subcmd=$1

fn setup -S http://127.0.0.1 > /dev/null 2>&1 || exit 1
#fn setup -S http://18.236.205.22

setup_env "custom" "root"

if [ "$subcmd" = "check" ]; then
  echo "Checking balance..."
  for address in $ADDRs;
  do
    echo "$address $(balance_of "$address")"
  done
  exit 0
fi

check_list=""
redo_list=""
counter=0
default_amount=$((2*1000*1000*1000*1000))
default_duration=13
default_fee=10000

for address in $ADDRs
do
  am=$default_amount
  duration=$default_duration
  fee=$default_fee
  expect=$(verified_deposit "$address" "$am" "$duration" "$fee")
  ret=$?
  if [ "$ret" -lt 10 ] && [ "$ret" -gt 0 ]; then
    redo_list="$redo_list $address"
  elif [ "$ret" -eq 100 ]; then
    check_list="$check_list $address,$expect"
  fi
  counter=$((counter+1))
  echo -n "$counter "
done

echo ""
echo "wait for another 5 seconds to check_result"
sleep 5

for item in $check_list
do
  address=$(echo "$item"|awk -F, '{print $1}')
  expect=$(echo "$item"|awk -F, '{print $2}')
  new=$(balance_of "$address")
  if [ "$new" != "$expect" ]; then
    redo_list="$redo_list $address"
  fi
done

if [ -n "$redo_list" ]; then
  echo "check done"
  echo "Second opportunity for: $redo_list"
fi

for address in $redo_list
do
  am=$default_amount
  duration=16
  verified_deposit "$address" "$am" "$duration"
  ret=$?
  if [ "$ret" -ne 0 ];then
    echo "give up $address"
  fi
done

echo "All Done"
