#!/usr/bin/env bash

set -e

num=$1
full_addr_book=$2
addr_book="$full_addr_book.address"

gen_one_key()
{
  eth_kp=$(fn gen-eth-key 2>&1 | head -3)

  mnemonic=$(echo "$eth_kp" | grep "Mnemonic" | awk '{ for(i=2;i<=NF;i++) printf $i" "}')
  pk=$(echo "$eth_kp" | grep "PrivateKey" | awk '{print $2}')
  addr=$(echo "$eth_kp" | grep "Address" | awk '{print $2}')

  printf "Mnemonic:%s\n" "$mnemonic"
  printf "PrivateKey:%s\n" "$pk"
  printf "Address:%s\n" "$addr"
}

for ((i=0; i<num; i++))
do
  gen_one_key >> "$full_addr_book"
  echo -n "$i "
done

grep "Address" "$full_addr_book"|awk -F: '{print $2}' > "$addr_book"