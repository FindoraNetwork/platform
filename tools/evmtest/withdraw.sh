#!/usr/bin/env bash

Eth_Mnemonic="walnut vapor math engage fiscal dumb mistake sort small only baby observe"
Eth_PrivateKey="84a1de1e39ad1eb373d7ace42898ee0d6fe2e16452e8334235bea84e47ec67d7"
Eth_Address="0xFA1FE96e836B785e0817F87Ca96fccfD62D39941"

Fra_Address="fra13qnw57dalxnfpvwrlwyyam0cu4mugvv6xg07790lzgj0s8kn8xlqen5969"
Fra_Mnemonic="actor pattern hole lunch thing roof praise air arrive east acoustic decorate slice squeeze author phrase prison robust catalog power pact abandon feel  coil"
Fra_Pk="iCbqeb35ppCxw_uITu345XfEMZoyH-8V_xIk-B7TOb4="
Fra_SK="Bj4EypLp0Mng8DfuoYKpZBikRxwj2lMKfLBwwCI8QiU="

source ./common/common.sh

env=$1
owner=$2

setup_env "$env" "$owner"|| exit

am=$((1000*1000*1000*1000))
duration=20
fee=10000
real=$(verified_deposit "$ADDR" "$am" "$duration" "$fee")
ret=$?
if [ "$ret" -ne 0 ]; then
  echo "failed to deposit $am($real) tokens to $Eth_Address"
  exit "$ret"
fi

fn contract-withdraw -a "$Fra_Address" -n 1000 -e "$Eth_Mnemonic"

balance_of "$Fra_Address"

#cleanup_env
