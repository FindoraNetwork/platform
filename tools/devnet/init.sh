#!/usr/bin/env bash

set -e

for ((i=0;i<=20;i++))
do
  cd /tmp/findora/devnet/node"$i"
  ls abci | grep -v abci|xargs -I {} rm -rf abci/{}
  ls data | grep -v priv_validator_state.json |xargs -I {} rm -rf data/{}
  echo -e '{\n  "height": "0",\n  "round": "0",\n  "step": 0\n}' > data/priv_validator_state.json
done