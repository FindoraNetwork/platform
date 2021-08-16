# Staking User Guide

1. Get the address of one or more nodes in FindoraNetwork
2. Set them in the `config.toml` of your node
3. Start your own node (findorad Application)
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
# switch to the `feat-testnet` branch
- make build DBG=1

# you should set up a cluster
# instead of a raw node in production env
# according to the official guidance of tendermint.

export ROOTDIR=/tmp/qa01-node
rm -rf ${ROOTDIR}/findora ${ROOTDIR}/tendermint
mkdir -p ${ROOTDIR}/findora ${ROOTDIR}/tendermint
findorad init --qa01-net --base-dir ${ROOTDIR}/tendermint
echo \
'
abci_host = "0.0.0.0"
abci_port = "26658"
tendermint_host = "0.0.0.0"
tendermint_port = "26657"
submission_host = "0.0.0.0"
submission_port = "8669"
ledger_host = "0.0.0.0"
ledger_port = "8668"
' > ${ROOTDIR}/findora/abci.toml

findorad node \
  --base-dir ${ROOTDIR}/tendermint \
  --config ${ROOTDIR}/tendermint/config/config.toml \
  --ledger-dir ${ROOTDIR}/findora \
  --tendermint-host 0.0.0.0 \
  --enable-ledger-service \
  --enable-query-service \
  --tendermint-node-key-config-path ${ROOTDIR}/tendermint/config/priv_validator_key.json \
  >> findorad.log 2>&1 &

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
# echo "[Your private mnemonic]" > ${ROOTDIR}/mnemonic.key
#
fns setup -O ${ROOTDIR}/mnemonic.key

# set the tendermint public key of your node
fns setup -K "${ROOTDIR}/tendermint/config/priv_validator_key.json"

# save a `staker_memo` file to current directory, for example.
cat staker_memo
{
  "name": "ExampleNode",
  "desc": "I am just a example description, please change me.",
  "website": "https://www.example.com",
  "logo": "https://www.example.com/logo"
}

# stake your node to FindoraNetwork,
# at least 1000000 FRAs are needed
fns stake -n $((100 * 10000 * 1000000)) -R 0.2 -M "$(cat staker_memo)"

# query your staking state after 10 minutes
fns show
```

```shell
$ fns -h
fns faddf8dd984b1ea5bbefa60c67e5c1980b913c89 2021-07-28
FindoraNetwork
A command line tool for staking in findora network.

USAGE:
    fns [SUBCOMMAND]

FLAGS:
    -h, --help       Prints help information
    -v, --version

SUBCOMMANDS:
    claim       Claim accumulated FRA rewards
    genkey      Generate a random Findora public key/private key Pair
    help        Prints this message or the help of the given subcommand(s)
    setup       Setup environment variables for staking transactions
    show        View Validator status and accumulated rewards
    stake       Stake tokens (i.e. bond tokens) from a Findora account to a Validator
    transfer    Transfer tokens from one address to another
    unstake     Unstake tokens (i.e. unbond tokens) from a Validator
```

```shell
$ fns stake -h
fns-stake
Stake tokens (i.e. bond tokens) from a Findora account to a Validator

USAGE:
    fns stake [FLAGS] [OPTIONS] --amount <Amount>

FLAGS:
    -a, --append     stake more FRAs to your node
        --force      ignore warning and stake FRAs to your node
    -h, --help       Prints help information
    -V, --version    Prints version information

OPTIONS:
    -n, --amount <Amount>                       how much `FRA unit`s you want to stake
    -R, --commission-rate <Rate>                the commission rate of your node, a float number from 0.0 to 1.0
    -S, --staker-priv-key <SecretKey>           the private key of proposer, in base64 format
    -M, --validator-memo <Memo>                 the description of your node, optional
    -A, --validator-td-addr <TendermintAddr>    stake FRAs to a custom validator
```
