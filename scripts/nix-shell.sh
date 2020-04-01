#!/usr/bin/env bash

protoc_path=$(readlink -f $(nix-shell -p protobuf --command "which protoc"))
export PROTOC="${protoc_path}"
export PROTOC_INCLUDE=$(dirname "${protoc_path}")/../include

exec nix-shell -p pkgconfig openssl binutils-unwrapped protobuf libgit2 curlFull zlib libssh2 yarn wasm-pack nodePackages.json-server --command zsh
