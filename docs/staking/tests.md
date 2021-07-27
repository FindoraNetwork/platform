# Staking Test

## Run Auto Cases

`make staking_test`

## Manual Tests

A successful `make debug_env` will start a local tendermint cluster, and pruduce a test tool called `stt`, and put it to `~/.cargo/bin`.

```shell
# stt --help

stt 0.1.0
FindoraNetwork
A manual test tool for the staking function.

USAGE:
    stt [SUBCOMMAND]

FLAGS:
    -h, --help       Prints help information
    -V, --version    Prints version information

SUBCOMMANDS:
    claim
    delegate
    help          Prints this message or the help of the given subcommand(s)
    init
    show
    transfer
    undelegate
```
