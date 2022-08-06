#!/usr/bin/env bash

platform=$1

if [ ! -f "./tools/tendermint.zip" ]; then
    if [ "MacOs" == "${platform}" ]; then
      echo "downloading... tendermint-macos"
      wget -O ./tools/tendermint.zip "https://github.com/tendermint/tendermint/releases/download/v0.33.9/tendermint_v0.33.9_darwin_amd64.zip"
    elif [ "Unix" == "${platform}" ]; then
      echo "downloading... tendermint-linux"
      wget -O ./tools/tendermint.zip "https://github.com/tendermint/tendermint/releases/download/v0.33.9/tendermint_v0.33.9_linux_amd64.zip"
    fi
else
    echo "tendermint.zip already exists."
fi

if ! [ -x "$(unzip -d ./tools/ ./tools/tendermint.zip)" ]; then
    echo 'please install unzip'
    exit
fi


