# Regression Test Environment

## Prerequisites
  - [x] The Rust toolchain
  - [x] Go 1.15+ installation or later to build tendermint(```0.33.5```)
  - [x] abcid + stt + fn
  - [x] Python 3.7.2+
  - [x] toml-cli: (Ubuntu: ```pip3 install toml-cli```)
  - [x] Web3.py: (```pip3 install web3```)
  - [x] openssl, leveldb, rocksdb: (Ubuntu: ```sudo apt install -y build-essential libleveldb-dev libssl-dev pkg-config clang libclang-dev librocksdb-dev```)

## Environment Variables
Below 3 environment variables can be customized in the file: `tools/devnet/env.sh`

URL | Default | Description
--- | --- | ---
BIN_CFG | `debug` | The version of binaries (fn, stt, etc.) to be used for tests
ENDPOINT | `http://0.0.0.0` | The endpoint of the environment to be tested
BLOCK_INTERVAL | `5` |The block time (in secs) of the environment to be tested

## Test Suites

command | Description
--- | ---
`make prismtest_nightly` | Perform prism tests and basic EVM tests by using fn and web3.py
`make tmtest_nightly` | Perform TripleMasking tests
