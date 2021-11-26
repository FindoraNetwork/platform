#!/usr/bin/env bash

set -e

source ./common/common.sh

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

if ! is_unsigned_integer "$num"; then
  echo "Not invalid count number"
  exit 1
fi

if [ "$full_addr_book" = "address" ]; then
  echo "Choose another file"
  exit 2
fi

for ((i=0; i<num; i++))
do
  gen_one_key >> "$full_addr_book"
  echo -n "$i "
done

grep "Address" "$full_addr_book"|awk -F: '{print $2}' > "$addr_book"