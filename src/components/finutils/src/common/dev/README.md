# `fn dev`

A powerful and convenient development tool for managing local clusters of FindoraNetwork.
Its goal is to replace some existing rudimentary development environment management tools, such as: `make debug_env` and `make devnet`.

## User guide

Through a `fn dev -h` we can see:

```shell
fn-dev
Manage development clusters on your localhost

USAGE:
    fn dev [OPTIONS] [SUBCOMMAND]

FLAGS:
    -h, --help       Prints help information
    -V, --version    Prints version information

OPTIONS:
    -e, --env-name <ENV NAME>    The name of the target env

SUBCOMMANDS:
    push-node      Attach a new node to an existing env
    create         Create a new env
    pop-node       Pop a node from an existing env
    destroy        Destroy an existing env
    destroy-all    Destroy all existing ENVs
    help           Prints this message or the help of the given subcommand(s)
    show           Default operation, show the information of an existing env
    show-all       Show the details of all existing ENVs
    init           Config the initial settings(POS,FRA issuance...)
    init-all       Apply the `init` operation to all existing ENVs
    list           List the names of all existing ENVs
    start          Start an existing env
    stop           Stop an existing env
```

#### Management of a single cluster

In the simplest scenario, similar to the functions of `make debug_env` and `make devnet`, we can:
- create and start a single cluster(`fn dev create`)
- stop the cluster(`fn dev stop`)
- restart/start the cluster(`fn dev start`)
- destroy the cluster(`fn dev destroy`)

However, `fn dev` can do far more than these, let's show a typical usage flow.

The first step, a new cluster need to be created by `fn dev create`, all configurations use default values, for example, the name of this ENV will be 'DEFAULT', the listening address of all nodes is '127.0.0.1', and so on.

Now you can do some basic tests on this new cluster, for example, check the availability of some APIs you care about:
- `curl 'localhost:8667/version'`
- `curl 'localhost:26657/validators'`
- ...

Then you are very likely to execute a `fn dev init` operation to initilize the 'DEFAULT' cluster, after this, all necessary system settings will be done.

Now the cluster can be considered to be a full-featured local network, you can:
- transfer tokens
- issue your custom tokens
- do staking operations
- do evm related operations
- ...

But wait,
- where to get FRA tokens?
- where to get the web-service ports of a target ENV?
- where to find all staking related keys of validators?
- ...

In a word, how to easily get all necessary information?

Don't worry, a `fn dev show` will show you everything you need, you can use a shorter style `fn dev` when using the default cluster, they are equal.

Below is the information of a custom ENV named 'MyEnv', `fn dev -e MyEnv`:
```json
{
  "env_name": "MyEnv",
  "env_home_dir": "/tmp/__FINDORA_DEV__/envs/MyEnv",
  "abcid_bin": "/tmp/abcid-v0.4.0-preview",
  "tendermint_bin": "/tmp/tendermint-v0.33.8",
  "abcid_extra_flags": "--disable-eth-empty-blocks",
  "tendermint_extra_flags": "validator",
  "host_ip": "192.168.2.5",
  "bank_account": {
    "wallet_address": "fra18xkez3fum44jq0zhvwq380rfme7u624cccn3z56fjeex6uuhpq6qv9e4g5",
    "public_key": "Oa2RRTzdayA8V2OBE7xp3n3NKrjGJxFTSZZybXOXCDQ=",
    "secret_key": "Ew9fMaryTL44ZXnEhcF7hQ-AB-fxgaC8vyCH-hCGtzg=",
    "mnemonic_words": "field ranch pencil chest effort coyote april move injury illegal forest amount bid sound mixture use second pet embrace twice total essay valve loan"
  },
  "initial_validator_number": 6,
  "initial_pos_settings": [
    {
      "tendermint_addr": "21D8A1DB770AF7C8CA0BD7BFE223DA5D5E79C238",
      "tendermint_pubkey": "YGj6VjFE3q0JkWnC2A+hI7S311khjOEMabIS3/RGwS4=",
      "xfr_keypair": {
        "pub_key": "HWQSmqnQFRmB5mEio5unslslFpdGC1Fg5PAXhfO8rs4=",
        "sec_key": "SgJLEsHB43dx4LVG93a1L4fdpvd9DJNlsZJ5jsyI-ms="
      },
      "xfr_mnemonic": "place pet blue notice normal custom hip waste fish brand assist east bright prepare slice shrimp top slow foot this hair river recycle vocal",
      "xfr_wallet_addr": "fra1r4jp9x4f6q23nq0xvy328xa8kfdj295hgc94zc8y7qtctuau4m8qpzl2qw"
    },
    {
      "tendermint_addr": "33A15A42B69531F9F0CC86E85FFE864131026DC2",
      "tendermint_pubkey": "+hIsyU5SN7Vkk7x5IAqnk9wLrVTsomkx3pEKTbjyM6g=",
      "xfr_keypair": {
        "pub_key": "EF-5Q9jyQvvMTri9Kj9ysgvzoVRugriwfoKDD7IguZc=",
        "sec_key": "rS2yiw8q7cnIlEUmsgP_LJRQ1ql2hiGQN702gTUfLwo="
      },
      "xfr_mnemonic": "fame nose practice thing fade sure casino torch furnace win emotion later discover spy observe found copper vicious piano ecology again brass pact orange",
      "xfr_wallet_addr": "fra1zp0mjs7c7fp0hnzwhz7j50mjkg9l8g25d6pt3vr7s2pslv3qhxtse6nmwa"
    },
    {
      "tendermint_addr": "72AA455D415F45E9C8D04840729815552693425B",
      "tendermint_pubkey": "++Glau+loOkKNYpgDClBR6uDvwtqeNnyYm7ieDP72+g=",
      "xfr_keypair": {
        "pub_key": "RDccnrOripudYGu0U1G4LFbsLQVFHJFLyMk583YXJlU=",
        "sec_key": "aXI1nfGe-qcYNV_e13HOCpDUYSPPiJaD1RqJRAi4wQ8="
      },
      "xfr_mnemonic": "donkey useless climb kiwi stumble link bubble easy elder supreme patch brave pretty target service neutral video lend cradle salute fold nuclear emotion submit",
      "xfr_wallet_addr": "fra1gsm3e84n4w9fh8tqdw69x5dc93twctg9g5wfzj7geyulxashye2shqygzr"
    },
    {
      "tendermint_addr": "950643B058A429CDAA8C21A3055135E62784B87E",
      "tendermint_pubkey": "KqHuzGEkLAmIARcTxIvrTnPD3a2AJJkZD3xW//eacG4=",
      "xfr_keypair": {
        "pub_key": "kBAE2HklcOeAYnj7XjMZwzFwCq-HxpXR0uuIB9x6vq8=",
        "sec_key": "9Q09QUFmXSM2Z0tH4sNOBIns0t_wHAzy1t953RSCoA4="
      },
      "xfr_mnemonic": "permit crack cigar expose clutch panel endless absurd story street fox night odor rigid cherry fly lawn jacket ostrich victory field art patch motion",
      "xfr_wallet_addr": "fra1jqgqfkrey4cw0qrz0ra4uvcecvchqz40slrft5wjawyq0hr6h6hs7efutr"
    },
    {
      "tendermint_addr": "B6979ECE8FE7C7E4BB4C663B8EEFB68CFE63F1AB",
      "tendermint_pubkey": "kIYP9VAuXw7psmvIG8gI/lBcrvjkHal1KtBGGdCfYeQ=",
      "xfr_keypair": {
        "pub_key": "D3H5SSU2KAXJIBnnERFQwnRN-e6rXYpArxNHtHdOb2g=",
        "sec_key": "7oM5MynASfSTqQkGKj0dsJaduNTdqoTPOLPbuL45Kfk="
      },
      "xfr_mnemonic": "hedgehog coconut derive pole defy virtual mother cactus game tourist globe moment shine shield fall wagon regret coffee outer crane extra announce push figure",
      "xfr_wallet_addr": "fra1pacljjf9xc5qtjfqr8n3zy2scf6ym70w4dwc5s90zdrmga6wda5qlawwrv"
    },
    {
      "tendermint_addr": "C0D10B2FFEDAC3D5E2DE130D7FD87262D1BAAFD4",
      "tendermint_pubkey": "udCBH/6XQ8bwP8Xh1MUrejBKx6cXHe6n/gmFxSmJZWU=",
      "xfr_keypair": {
        "pub_key": "8Mjy6lspvMoZ4CxIXKSWP-vRsY-ldDhYggHUqDRxC7g=",
        "sec_key": "EuvYmOvoaPyYVR7IkTSxIUYhdFPc3AZxB-SzuMpQMAA="
      },
      "xfr_mnemonic": "pole woman magic fox initial misery female moral entire tent pistol language drum embrace adapt leisure exact lens budget fit about often foil pyramid",
      "xfr_wallet_addr": "fra17ry096jm9x7v5x0q93y9efyk8l4arvv0546rskyzq822sdr3pwuql8vavc"
    }
  ],
  "block_interval": 1,
  "evm_chain_id": 777,
  "checkpoint_file": "/tmp/checkpoint.toml",
  "seed_nodes": {
    "0": {
      "id": 0,
      "tendermint_node_id": "a2d8b4efeb73b2b1d19edfa34d37f53c1f0b6305",
      "node_home_dir": "/tmp/__FINDORA_DEV__/envs/MyEnv/0",
      "kind": "Seed",
      "occupied_ports": {
        "web3_http_service": 26492,
        "web3_websocket_service": 35589,
        "tendermint_p2p_service": 22372,
        "tendermint_rpc_service": 50788,
        "abcid_abci_service": 21695,
        "abcid_submission_service": 31755,
        "abcid_ledger_query_service": 52745
      }
    }
  },
  "validator_or_full_nodes": {
    "1": {
      "id": 1,
      "tendermint_node_id": "83eed56e18f33399911857249cb5f6674d140f61",
      "node_home_dir": "/tmp/__FINDORA_DEV__/envs/MyEnv/1",
      "kind": "ValidatorOrFull",
      "occupied_ports": {
        "web3_http_service": 37637,
        "web3_websocket_service": 26357,
        "tendermint_p2p_service": 27563,
        "tendermint_rpc_service": 23568,
        "abcid_abci_service": 38234,
        "abcid_submission_service": 54814,
        "abcid_ledger_query_service": 27921
      }
    },
    "2": {
      "id": 2,
      "tendermint_node_id": "513a31b3b414865a8b336ba90dfcee2b86ed8653",
      "node_home_dir": "/tmp/__FINDORA_DEV__/envs/MyEnv/2",
      "kind": "ValidatorOrFull",
      "occupied_ports": {
        "web3_http_service": 65261,
        "web3_websocket_service": 22836,
        "tendermint_p2p_service": 58687,
        "tendermint_rpc_service": 42012,
        "abcid_abci_service": 36935,
        "abcid_submission_service": 34056,
        "abcid_ledger_query_service": 36764
      }
    },
    "3": {
      "id": 3,
      "tendermint_node_id": "cea785e325d76f2eb339e933f8d213acb4514c77",
      "node_home_dir": "/tmp/__FINDORA_DEV__/envs/MyEnv/3",
      "kind": "ValidatorOrFull",
      "occupied_ports": {
        "web3_http_service": 29799,
        "web3_websocket_service": 35889,
        "tendermint_p2p_service": 58781,
        "tendermint_rpc_service": 21344,
        "abcid_abci_service": 38653,
        "abcid_submission_service": 30465,
        "abcid_ledger_query_service": 47821
      }
    },
    "4": {
      "id": 4,
      "tendermint_node_id": "ee19df1f8405dee803197c6e0cabcc56f24231b0",
      "node_home_dir": "/tmp/__FINDORA_DEV__/envs/MyEnv/4",
      "kind": "ValidatorOrFull",
      "occupied_ports": {
        "web3_http_service": 38117,
        "web3_websocket_service": 29534,
        "tendermint_p2p_service": 36869,
        "tendermint_rpc_service": 32051,
        "abcid_abci_service": 27732,
        "abcid_submission_service": 21521,
        "abcid_ledger_query_service": 41027
      }
    },
    "5": {
      "id": 5,
      "tendermint_node_id": "0ade68e5f29b31ad35a1ab41802740eedc7a38de",
      "node_home_dir": "/tmp/__FINDORA_DEV__/envs/MyEnv/5",
      "kind": "ValidatorOrFull",
      "occupied_ports": {
        "web3_http_service": 35378,
        "web3_websocket_service": 22092,
        "tendermint_p2p_service": 55933,
        "tendermint_rpc_service": 34649,
        "abcid_abci_service": 53370,
        "abcid_submission_service": 31045,
        "abcid_ledger_query_service": 29152
      }
    },
    "6": {
      "id": 6,
      "tendermint_node_id": "099f9f5c45c46c65a0fe6dcc911cd6ff09b8c40a",
      "node_home_dir": "/tmp/__FINDORA_DEV__/envs/MyEnv/6",
      "kind": "ValidatorOrFull",
      "occupied_ports": {
        "web3_http_service": 51964,
        "web3_websocket_service": 35454,
        "tendermint_p2p_service": 32413,
        "tendermint_rpc_service": 53797,
        "abcid_abci_service": 36495,
        "abcid_submission_service": 31620,
        "abcid_ledger_query_service": 43557
      }
    }
  },
  "next_node_id": 7,
  "tendermint_genesis_config": "{\"app_hash\":\"\",\"chain_id\":\"test-chain-GIEbMA\",\"consensus_params\":{\"block\":{\"max_bytes\":\"22020096\",\"max_gas\":\"-1\",\"time_iota_ms\":\"1000\"},\"evidence\":{\"max_age_duration\":\"172800000000000\",\"max_age_num_blocks\":\"100000\"},\"validator\":{\"pub_key_types\":[\"ed25519\"]}},\"genesis_time\":\"2022-09-12T13:26:18.175795739Z\",\"validators\":[{\"address\":\"950643B058A429CDAA8C21A3055135E62784B87E\",\"name\":\"node-0\",\"power\":\"1\",\"pub_key\":{\"type\":\"tendermint/PubKeyEd25519\",\"value\":\"KqHuzGEkLAmIARcTxIvrTnPD3a2AJJkZD3xW//eacG4=\"},\"voting_power\":\"1\"},{\"address\":\"72AA455D415F45E9C8D04840729815552693425B\",\"name\":\"node-1\",\"power\":\"1\",\"pub_key\":{\"type\":\"tendermint/PubKeyEd25519\",\"value\":\"++Glau+loOkKNYpgDClBR6uDvwtqeNnyYm7ieDP72+g=\"},\"voting_power\":\"1\"},{\"address\":\"33A15A42B69531F9F0CC86E85FFE864131026DC2\",\"name\":\"node-2\",\"power\":\"1\",\"pub_key\":{\"type\":\"tendermint/PubKeyEd25519\",\"value\":\"+hIsyU5SN7Vkk7x5IAqnk9wLrVTsomkx3pEKTbjyM6g=\"},\"voting_power\":\"1\"},{\"address\":\"B6979ECE8FE7C7E4BB4C663B8EEFB68CFE63F1AB\",\"name\":\"node-3\",\"power\":\"1\",\"pub_key\":{\"type\":\"tendermint/PubKeyEd25519\",\"value\":\"kIYP9VAuXw7psmvIG8gI/lBcrvjkHal1KtBGGdCfYeQ=\"},\"voting_power\":\"1\"},{\"address\":\"C0D10B2FFEDAC3D5E2DE130D7FD87262D1BAAFD4\",\"name\":\"node-4\",\"power\":\"1\",\"pub_key\":{\"type\":\"tendermint/PubKeyEd25519\",\"value\":\"udCBH/6XQ8bwP8Xh1MUrejBKx6cXHe6n/gmFxSmJZWU=\"},\"voting_power\":\"1\"},{\"address\":\"21D8A1DB770AF7C8CA0BD7BFE223DA5D5E79C238\",\"name\":\"node-5\",\"power\":\"1\",\"pub_key\":{\"type\":\"tendermint/PubKeyEd25519\",\"value\":\"YGj6VjFE3q0JkWnC2A+hI7S311khjOEMabIS3/RGwS4=\"},\"voting_power\":\"1\"}]}"
}
```

You can pause the cluster by `fn dev stop`, and resume it by `fn dev start` at any time; you can also scale up the cluster by `fn dev push-node`, and scale it down by `fn dev pop-node`.

At last, if you don't need this cluster anymore, you can permanently destroy it with the `fn dev destroy` subcommand.

The above is the simplest management process of a local development environment, which is enough for developers to self-debug on their localhosts.

But obviously, for example, for the scenario of front-end and back-end joint debugging, the simplest cluster configuration above is not enough, so we need some additional configuration options to meet these requirements. Most of these additional configurations need to be specified during the cluster creation process, that is, specified in the scope of the `fn dev create` subcommand.

Let's check the help information of `fn dev create`:
```
fn-dev-create
Create a new env

USAGE:
    fn dev create [FLAGS] [OPTIONS]

FLAGS:
    -f, --force      destroy the target ENV and create a new one
    -h, --help       Prints help information
    -V, --version    Prints version information

OPTIONS:
    -d, --abcid-bin-path <ABCID BIN PATH>                    The path of your custom abcid binary
    -x, --abcid-extra-flags <ABCID EXTRA FLAGS>              A pair of quotes should be used when specifying extra flags
    -i, --block-itv-secs <BLOCK INTERVAL>                    Block interval in seconds
    -c, --checkpoint-file <CHECKPOINT FILE>                  The file path of the checkpoint file
    -e, --env-name <ENV NAME>                                The name of the target env
    -I, --evm-chain-id <EVM CHAIN ID>                        The chain id in the scope of evm logic
    -H, --host-ip <HOST IP>                                  The IP of your local host, default to 127.0.0.1
    -D, --tendermint-bin-path <TENDERMINT BIN PATH>          The path of your custom tendermint binary
    -X, --tendermint-extra-flags <TENDERMINT EXTRA FLAGS>    A pair of quotes should be used when specifying extra flags
    -N, --validator-num <VALIDATOR NUMBER>                   How many initial validators should be created
```

For the issue of remote joint debugging, we can use the `--host-ip` option to specify the listening address of the target ENV.

A few other commonly used options:
- `-i, --block-itv-secs`, block interval, default to 3s
- `-c, --checkpoint-file`, the path of you custom checkpoint file
- `-I, --evm-chain-id`, the value of this option will be defined as `${EVM_CHAIN_ID}`
- `-N, --validator-num`, 5 initial validators will be created by default, you can change the number by this option
- `-d, --abcid-bin-path`, use a custom versioln of the abcid binary
- `-D, --tendermint-bin-path`, use a custom version of the tendermint binary
- `-x, --abcid-extra-flags`, specify extra flags for abcid
  - for example, there is a `--disable-eth-empty-blocks` flag in the `abcid` binary, and it will not be set by `fn dev` by default, you can set it like this `fn dev create -x '--disable-eth-empty-blocks'`
- `-X, --tendermint-extra-flags`, specify extra flags for tendermint

Below is a more complete example with richer options:
```shell
fn dev create \
    -H 192.168.2.5 \
    -e MyEnv \
    -i 1 \
    -N 6 \
    -I 777 \
    -c /tmp/checkpoint.toml \
    -d /tmp/abcid-v0.4.0-preview \
    -D /tmp/tendermint-v0.33.8 \
    -x '--disable-eth-empty-blocks' \
    -X 'validator'
```

- all nodes of this ENV will listen on '192.168.2.5'
  - now you can tell the IP address to your frond-end engineers, the joint debugging will be ok
- the name of this ENV is 'MyEnv'
- the block interval will be 1s
- the number of initial validator nodes is 6
- the chain id of evm is 777
- the path of checkpoint file is '/tmp/checkpoint.toml'
- use custom binaries of tendermint and abcid
- '--disable-eth-empty-blocks' is added as an extra flag for abcid
- 'validator' is added as an extra flag for tendermint process

The coresponding starting commands of nodes will be(but in one-line style):
```shell
# cat /tmp/__FINDORA_DEV__/envs/MyEnv/3/fn_dev.log

/tmp/tendermint-v0.33.8 node \
    --home /tmp/__FINDORA_DEV__/envs/MyEnv/3 validator \
    validator \
    >>/tmp/__FINDORA_DEV__/envs/MyEnv/3/tendermint.log 2>&1 &

EVM_CHAIN_ID=777 FINDORA_BLOCK_ITV=1 \
/tmp/abcid-v0.4.0-preview --enable-query-service --enable-eth-api-service \
    --tendermint-host 192.168.2.5 \
    --tendermint-port 41043 \
    --abcid-port 22480 \
    --submission-service-port 61919 \
    --ledger-service-port 61912 \
    --evm-http-port 21536 \
    --evm-ws-port 34983 \
    --ledger-dir /tmp/__FINDORA_DEV__/envs/MyEnv/3/__findora__ \
    --tendermint-node-key-config-path /tmp/__FINDORA_DEV__/envs/MyEnv/3/config/priv_validator_key.json \
    --disable-eth-empty-blocks \
    --checkpoint-file /tmp/checkpoint.toml \
    >>/tmp/__FINDORA_DEV__/envs/MyEnv/3/app.log 2>&1 &
```

#### Management of multiple clusters

Since each cluster can specify its own executing binaries(tendermint & abcid), the multi-cluster mode is of great significance for functional comparison, testing and problem debugging between different versions or between different features.

Managing multiple clusters, or in other words, managing custom clusters is not much different from the default cluster because resource allocation and process running between different clusters are completely isolated in `fn dev`.

The only difference is that you do not have to explicitly specify the env name when managing the default cluster, but for non-default clusters, all operations must explicitly specify the name of the target env.

For example, for the default cluster, `fn dev stop` is equal to `fn dev stop -e DEFAULT`, both styles are ok; but there is only one style for a custom cluster, that is `fn dev stop -e YourCustomEnv`.

Also, there are some subcommands designed specifically for multi-cluster management:
- `fn dev list`, list the names of all existing ENVs
- `fn dev show-all`, list the details of all existing ENVs
- `fn dev init-all`, initilize all existing ENVs in batch mode
- `fn dev destroy-all`, destroy all existing ENVs

#### Environment variable definitions

Currently no environment variables are defined inside `fn dev`.

#### Internal organization of data and logs

All data and logs are located under `/tmp/__FINDORA_DEV__`, so you should have a big enough `/tmp`.

We can use `tree -F -L 2 /tmp/__FINDORA_DEV__` to check their structures:
```
/tmp/__FINDORA_DEV__/
├── envs/               # existing ENVs
│   ├── DEFAULT/        # the default ENV
│   ├── env_A/          # a custom ENV named 'env_A'
│   └── env_B/          # a custom ENV named 'env_B'
└── ports_cache         # allocated ports
```

Let's check the inner structure of 'DEFAULT', `tree -F -L 1 /tmp/__FINDORA_DEV__/envs/DEFAULT`:
```
/tmp/__FINDORA_DEV__/envs/DEFAULT/
├── 0/           # seed node of this ENV, can *not* be removed dynamicly
├── 1/           # the first validator node of this ENV, can *not* be removed dynamicly
├── 2/           # the second validator node of this ENV, can be removed dynamicly
├── 3/           # the third validator node, similar with the second one
├── 4/           # ...
├── 5/           # ...
└── config.json  # config file of this ENV
```

Then further check the internal structure of a node, `tree -F -L 1 /tmp/__FINDORA_DEV__/envs/DEFAULT/1`:

```
/tmp/__FINDORA_DEV__/envs/DEFAULT/1/
├── config/              # config dir of the tendermint consensus engine
├── data/                # data dir of the tendermint consensus engine
├── tendermint.log       # log of the tendermint consensus engine
├── __findora__/         # data of the 'abcid' process
├── app.log              # log of the 'abcid' process
└── fn_dev.log           # log of the 'fn dev' system
```

Inner management operations of `fn dev` will be logged in the `fn_dev.log` file.

## Compatibility Notes

#### OS compatibility

In theory, it can run well on most Linux and MacOS versions, but it is not suitable for Windows operating systems, and there is currently no compatibility plan for Windows.

#### No breaks to musl compilation

```shell
# host: Linux 5.15.52-gentoo #2 SMP Tue Jul 26 15:14:31 CST 2022 x86_64  GNU/Linux

X86_64_UNKNOWN_LINUX_MUSL_OPENSSL_LIB_DIR=/usr/x86_64-unknown-linux-musl/usr/lib \
X86_64_UNKNOWN_LINUX_MUSL_OPENSSL_INCLUDE_DIR=/usr/x86_64-unknown-linux-musl/usr/include \
\
cargo build --target=x86_64-unknown-linux-musl -p finutils
```
