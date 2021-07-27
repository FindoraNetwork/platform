#!/usr/bin/env bash

target_path=$1

if [[ ! -d "${target_path}/.git" ]]; then
    rm -rf $target_path
    url='https://gitee.com/findora-network/tendermint.git'
    if [[ 0 -eq `date | grep -c CST` ]]; then
        url='https://github.com/FindoraNetwork/tendermint.git'
    fi
    git clone -b feat-findora --depth=1 $url $target_path
fi
