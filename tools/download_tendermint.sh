#!/usr/bin/env bash

target_path=$1

if [[ ! -d "${target_path}/.git" ]]; then
    rm -rf $target_path
    url='https://github.com/FindoraNetwork/tendermint.git'
    git clone -b findora-v0.33.9_consensus --depth=1 $url $target_path
fi
