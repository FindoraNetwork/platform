#!/usr/bin/env bash

platform=$1

if [ ! -f "./tools/tendermint.zip" ]; then
  if [ "MacOS" == "${platform}" ]; then
    wget -O ./tools/tendermint.zip "https://github.com/tendermint/tendermint/releases/download/v0.33.9/tendermint_v0.33.9_darwin_amd64.zip"
  elif [ "Linux" == "${platform}" ]; then
    wget -O ./tools/tendermint.zip "https://github.com/tendermint/tendermint/releases/download/v0.33.9/tendermint_v0.33.9_linux_amd64.zip"
  fi
else
  echo "tendermint.zip already exists."
fi

if ! [ -x "$(command -v unzip)" ]; then
  echo 'error:'
  echo 'please install unzip.' >&2
  exit 1
else
  unzip -d ./tools/ ./tools/tendermint.zip
fi

if [ ! -f "./tools/tendermint" ]; then
  rm ./tools/tendermint.zip
  rm ./tools/tendermint
  echo 'error:'
  echo 'unzip ./tools/tendermint.zip failed.'
  echo 'Make sure ./tools/tendermint.zip is a complete file, or try again.' >&2
  exit 1
fi




