# Staking User Guide

1. Get the address of one or more nodes in FindoraNetwork
2. Set them in the `config.toml` of your node
3. Start your own node (ABCI Application + Tendermint Core)
4. Stake your node to FindoraNetwork
    - It will become a candidate validator at once after a successful staking
    - It will become an official validator if the staking amount is sufficient
5. Append new stakings to raise the vote power of your node

## Example

```shell
**You can download (outdated)**:

- Linux
    - https://github.com/FindoraNetwork/iii/releases/download/fnstest/fns.linux
    - https://github.com/FindoraNetwork/iii/releases/download/fnstest/tendermint.linux
    - https://github.com/FindoraNetwork/iii/releases/download/fnstest/abci_validator_node.linux
- MacOS
    - https://github.com/FindoraNetwork/iii/releases/download/fnstest/fns.macos
    - https://github.com/FindoraNetwork/iii/releases/download/fnstest/tendermint.macos
    - https://github.com/FindoraNetwork/iii/releases/download/fnstest/abci_validator_node.macos

**Or compile from scratch**:

# use `make` directly in production env
# switch to the `feat-staking-v2-DebugEnv` branch
- make build DBG=1

# you should set up a cluster
# instead of a raw node in production env
# according to the official guidance of tendermint.

# Many popular linux distribution such as Ubuntu 20.04 and MacOs ship with Python pre-installed.
# To install `toml` tool, you need install pip first, a tool that install and manage software packages
# for Python. To install toml, run in your shell:

- pip install toml-cli

#
# add addresses of some existing nodes
#   - <NODE ID>@https://prod-mainnet-us-west-2-sentry-000-public.prod.findora.org:<PORT>
#   - <NODE ID>@https://prod-mainnet-us-west-2-sentry-001-public.prod.findora.org:<PORT>
#
rm -rf /tmp/findora ~/.tendermint
tendermint init

curl https://dev-qa01.dev.findora.org:26657/genesis \
    | jq -c '.result.genesis' \
    | jq > ~/.tendermint/config/genesis.json

toml set   --toml-path ~/.tendermint/config/config.toml consensus.create_empty_blocks_interval 15s
toml set   --toml-path ~/.tendermint/config/config.toml p2p.persistent_peers \
"b87304454c0a0a0c5ed6c483ac5adc487f3b21f6@dev-qa01-us-west-2-sentry-000-public.dev.findora.org:26656,\
d0c6e3e1589695ae6d650b288caf2efe9a998a50@dev-qa01-us-west-2-sentry-001-public.dev.findora.org:26656"

TD_NODE_SELF_ADDR=$(jq --raw-output '.address' ~/.tendermint/config/priv_validator_key.json) \
    LEDGER_DIR=/tmp/findora \
    abci_validator_node >/tmp/log 2>&1 &

nohup tendermint node > /tmp/td.log 2>&1 &

# set the server address,
# should be the address of an existing node
#
# the easiest way is to use the community address
#   - https://prod-mainnet.prod.findora.org
fns setup -S https://dev-qa01.dev.findora.org

# set your mnemonic key which can be got from wallet
#
# NOTE:
# you should use an existing key file instead of `echo` for security in your production env
#
# echo "[Your private mnemonic]" > $(pwd)/mnemonic.key
#
fns setup -O $(pwd)/mnemonic.key

# set the tendermint public key of your node
fns setup -K "${HOME}/.tendermint/config/priv_validator_key.json"

# stake your node to FindoraNetwork,
# at least 1000000 FRAs are needed
fns stake -n $((100 * 10000 * 1000000)) -R 0.2 -M "my node"

# query your staking state after 10 minutes
fns show
```

```shell
fns 0.1.0
FindoraNetwork
A command line tool for staking in findora network.

USAGE:
    fns [SUBCOMMAND]

FLAGS:
    -h, --help       Prints help information
    -V, --version    Prints version information

SUBCOMMANDS:
    claim
    contribute
    help                      Prints this message or the help of the given subcommand(s)
    set-initial-validators
    setup
    show
    stake
    transfer
    unstake
```

```shell
fns-stake

USAGE:
    fns stake [FLAGS] [OPTIONS] --amount <Amount>

FLAGS:
    -a, --append     stake more FRAs to your node
    -h, --help       Prints help information
    -V, --version    Prints version information

OPTIONS:
    -n, --amount <Amount>           how much `FRA unit`s you want to stake
    -R, --commission-rate <Rate>    the commission rate for your delegators, should be a float numbe
    -M, --validator-memo <Memo>     the description of your validator node, optional
```
