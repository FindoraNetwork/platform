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
    create         Create a new env
    destroy        Destroy an existing env
    destroy-all    Destroy all existing ENVs
    help           Prints this message or the help of the given subcommand(s)
    init           Config the initial settings(POS,FRA issuance...)
    init-all       Apply the `init` operation to all existing ENVs
    list           List the names of all existing ENVs
    pop-node       Pop a node from an existing env
    push-node      Attach a new node to an existing env
    show           Default operation, show the information of an existing env
    show-all       Show the details of all existing ENVs
    start          Start an existing env
    start-all      Start all existing ENVs
    stop           Stop an existing env
    stop-all       Stop all existing ENVs
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
  "meta": {
    "env_name": "MyEnv",
    "env_home_dir": "/tmp/__CHAIN_DEV__/7773x/fh/__DEV__/envs/MyEnv",
    "host_ip": "192.168.2.5",
    "app_bin_path": "/tmp/abcid-v0.4.0-preview",
    "app_extra_opts": "--disable-eth-empty-blocks",
    "tendermint_bin_path": "/tmp/tendermint-v0.33.8",
    "tendermint_extra_opts": "validator",
    "block_interval_in_seconds": 1.0,
    "seed_nodes": {
      "0": {
        "id": 0,
        "tendermint_node_id": "4663d3cb408a56218cbc470a3ef5b3756ae865a7",
        "node_home_dir": "/tmp/__CHAIN_DEV__/7773x/fh/__DEV__/envs/MyEnv/0",
        "kind": "Seed",
        "ports": {
          "web3_http_service": 40490,
          "web3_websocket_service": 28126,
          "tendermint_p2p_service": 23245,
          "tendermint_rpc_service": 21675,
          "abcid_abci_service": 45977,
          "abcid_ledger_query_service": 23949,
          "abcid_submission_service": 30603
        }
      }
    },
    "validator_or_full_nodes": {
      "1": {
        "id": 1,
        "tendermint_node_id": "fa79b757b3f09f2be5f8938f0484aa66a4a28650",
        "node_home_dir": "/tmp/__CHAIN_DEV__/7773x/fh/__DEV__/envs/MyEnv/1",
        "kind": "ValidatorOrFull",
        "ports": {
          "web3_http_service": 57260,
          "web3_websocket_service": 56007,
          "tendermint_p2p_service": 44812,
          "tendermint_rpc_service": 49076,
          "abcid_abci_service": 60803,
          "abcid_ledger_query_service": 21010,
          "abcid_submission_service": 48445
        }
      },
      "2": {
        "id": 2,
        "tendermint_node_id": "436358d10f45d3a7d45ee0818f6fe3f77667661e",
        "node_home_dir": "/tmp/__CHAIN_DEV__/7773x/fh/__DEV__/envs/MyEnv/2",
        "kind": "ValidatorOrFull",
        "ports": {
          "web3_http_service": 64973,
          "web3_websocket_service": 51868,
          "tendermint_p2p_service": 32920,
          "tendermint_rpc_service": 65262,
          "abcid_abci_service": 29600,
          "abcid_ledger_query_service": 62523,
          "abcid_submission_service": 21203
        }
      },
      "3": {
        "id": 3,
        "tendermint_node_id": "0946c633b2095c46aa23984f455b50bf05dd0283",
        "node_home_dir": "/tmp/__CHAIN_DEV__/7773x/fh/__DEV__/envs/MyEnv/3",
        "kind": "ValidatorOrFull",
        "ports": {
          "web3_http_service": 34351,
          "web3_websocket_service": 37564,
          "tendermint_p2p_service": 35864,
          "tendermint_rpc_service": 51267,
          "abcid_abci_service": 33204,
          "abcid_ledger_query_service": 35230,
          "abcid_submission_service": 54066
        }
      },
      "4": {
        "id": 4,
        "tendermint_node_id": "ca06bd19e0dcdba902aaf6e933415aa94359f295",
        "node_home_dir": "/tmp/__CHAIN_DEV__/7773x/fh/__DEV__/envs/MyEnv/4",
        "kind": "ValidatorOrFull",
        "ports": {
          "web3_http_service": 30729,
          "web3_websocket_service": 38228,
          "tendermint_p2p_service": 40293,
          "tendermint_rpc_service": 56962,
          "abcid_abci_service": 38108,
          "abcid_ledger_query_service": 38105,
          "abcid_submission_service": 23464
        }
      },
      "5": {
        "id": 5,
        "tendermint_node_id": "013f28322d5ac75364b266b55ff59b3b1231c080",
        "node_home_dir": "/tmp/__CHAIN_DEV__/7773x/fh/__DEV__/envs/MyEnv/5",
        "kind": "ValidatorOrFull",
        "ports": {
          "web3_http_service": 31284,
          "web3_websocket_service": 27054,
          "tendermint_p2p_service": 26567,
          "tendermint_rpc_service": 22058,
          "abcid_abci_service": 26502,
          "abcid_ledger_query_service": 29906,
          "abcid_submission_service": 21829
        }
      },
      "6": {
        "id": 6,
        "tendermint_node_id": "05c7fb928ce98b64f4f343fc020ac5073f057b61",
        "node_home_dir": "/tmp/__CHAIN_DEV__/7773x/fh/__DEV__/envs/MyEnv/6",
        "kind": "ValidatorOrFull",
        "ports": {
          "web3_http_service": 40754,
          "web3_websocket_service": 62700,
          "tendermint_p2p_service": 41046,
          "tendermint_rpc_service": 27463,
          "abcid_abci_service": 34729,
          "abcid_ledger_query_service": 38627,
          "abcid_submission_service": 39514
        }
      }
    },
    "tendermint_genesis": {
      "app_hash": "",
      "app_state": null,
      "chain_id": "test-chain-6NUq95",
      "consensus_params": {
        "block": {
          "max_bytes": "22020096",
          "max_gas": "-1",
          "time_iota_ms": "1000"
        },
        "evidence": {
          "max_age_duration": "172800000000000",
          "max_age_num_blocks": "100000"
        },
        "validator": {
          "pub_key_types": [
            "ed25519"
          ]
        }
      },
      "genesis_time": "2022-10-25T13:15:38.0010803Z",
      "validators": [
        {
          "address": "4AF253F678AAA5B2A729B9E1E67559664AD927ED",
          "name": "node-0",
          "power": "1000000000",
          "pub_key": {
            "type": "tendermint/PubKeyEd25519",
            "value": "IkqdVRNXUX0wvJprecDbrNuix+6IE0rFCKwKn4wB78E="
          }
        },
        {
          "address": "417D77726F9146F011319AA1A62B5FB62B1C5C3C",
          "name": "node-1",
          "power": "1000000000",
          "pub_key": {
            "type": "tendermint/PubKeyEd25519",
            "value": "wQ+KCiwwnA+29PFgRvCI/xFQEUn+p9fNo5Qzq5fZwoY="
          }
        },
        {
          "address": "67210EF0A9364620AC94F5522872AD3D226DAD47",
          "name": "node-2",
          "power": "1000000000",
          "pub_key": {
            "type": "tendermint/PubKeyEd25519",
            "value": "OouftWmVbWxXHx1y1x66DxEZtKqh2cJjHTXQ9zBp1uI="
          }
        },
        {
          "address": "B5D1AF330440FCE312048EF6CA33A9553C4265AE",
          "name": "node-3",
          "power": "1000000000",
          "pub_key": {
            "type": "tendermint/PubKeyEd25519",
            "value": "8kYdjNcAFI0abbVzKywfLzX7y3sd/jDalQaZiz2z3KY="
          }
        },
        {
          "address": "0A6204F24C93E73E1ED79890A65911E4306A183B",
          "name": "node-4",
          "power": "1000000000",
          "pub_key": {
            "type": "tendermint/PubKeyEd25519",
            "value": "upYe1bDrURVaD+c1LqP2srPeK5g+MKvWWEf7jrY4evo="
          }
        },
        {
          "address": "9E6F4C869F58795CFC5E35B8782FBFD8198D167B",
          "name": "node-5",
          "power": "1000000000",
          "pub_key": {
            "type": "tendermint/PubKeyEd25519",
            "value": "LgiZAPZ2+Oayw1mf0nXJmMFAgw0pe7A6qzVOqeF7V/o="
          }
        }
      ]
    },
    "custom_data": {
      "evm_chain_id": 777,
      "checkpoint_file": "/tmp/checkpoint.toml",
      "bank_account": {
        "wallet_address": "fra18xkez3fum44jq0zhvwq380rfme7u624cccn3z56fjeex6uuhpq6qv9e4g5",
        "public_key": "Oa2RRTzdayA8V2OBE7xp3n3NKrjGJxFTSZZybXOXCDQ=",
        "secret_key": "Ew9fMaryTL44ZXnEhcF7hQ-AB-fxgaC8vyCH-hCGtzg=",
        "mnemonic_words": "field ranch pencil chest effort coyote april move injury illegal forest amount bid sound mixture use second pet embrace twice total essay valve loan"
      },
      "initial_validator_num": 6,
      "initial_pos_settings": [
        {
          "tendermint_addr": "0A6204F24C93E73E1ED79890A65911E4306A183B",
          "tendermint_pubkey": "upYe1bDrURVaD+c1LqP2srPeK5g+MKvWWEf7jrY4evo=",
          "xfr_keypair": {
            "pub_key": "SVK6Ej78fUKOzKNbSwwlxBoVgUft4TSGT069D9ToG_I=",
            "sec_key": "Bag_bgX67nVACkurB9uJWxW1WF2hKiCtjlHKBHev-IQ="
          },
          "xfr_mnemonic": "foil shiver mass proof unhappy embrace engine mosquito tortoise giggle deputy climb tape vocal someone toe minute upper hurry tongue firm easy honey pride",
          "xfr_wallet_addr": "fra1f9ft5y37l3759rkv5dd5krp9csdptq28ahsnfpj0f67sl48gr0eqnf3zqs"
        },
        {
          "tendermint_addr": "417D77726F9146F011319AA1A62B5FB62B1C5C3C",
          "tendermint_pubkey": "wQ+KCiwwnA+29PFgRvCI/xFQEUn+p9fNo5Qzq5fZwoY=",
          "xfr_keypair": {
            "pub_key": "mAeMICthEJgg5RmiqQpRy4lmkBYv9Ds7738kCIFGVSM=",
            "sec_key": "dg_AuFzwSEtqPGXoq6RSZkQabgjidRTTbnXLysrZFFc="
          },
          "xfr_mnemonic": "beyond praise gun help accuse special fall vessel return shaft trouble vendor episode airport become dilemma strategy pony trial light child wedding tennis true",
          "xfr_wallet_addr": "fra1nqrccgptvygfsg89rx32jzj3ewykdyqk9l6rkwl00ujq3q2x253sczvuxn"
        },
        {
          "tendermint_addr": "4AF253F678AAA5B2A729B9E1E67559664AD927ED",
          "tendermint_pubkey": "IkqdVRNXUX0wvJprecDbrNuix+6IE0rFCKwKn4wB78E=",
          "xfr_keypair": {
            "pub_key": "yU5Uw5XHeQ5ZJ7KP2U48T3qhD1pPhtm0dbPxmcjB9UA=",
            "sec_key": "v1Tyw6TUhgpYbU_j0EArUSA_GQaufem4lnbOjoG-uBw="
          },
          "xfr_mnemonic": "sister talk breeze brave mammal laundry clinic age mutual quarter never coffee witness fiscal school mom ancient rival erode raccoon seed addict unknown balcony",
          "xfr_wallet_addr": "fra1e989fsu4causukf8k28ajn3ufaa2zr66f7rdndr4k0cenjxp74qqqhf5e5"
        },
        {
          "tendermint_addr": "67210EF0A9364620AC94F5522872AD3D226DAD47",
          "tendermint_pubkey": "OouftWmVbWxXHx1y1x66DxEZtKqh2cJjHTXQ9zBp1uI=",
          "xfr_keypair": {
            "pub_key": "GfO0pnFheTRAsemWC2NmzO0A6A5IHKl0dk4q0KzIsPI=",
            "sec_key": "Z6mq53Cp1WteqO6GTHBAFFNXzmUg1f-9WQWcP-O67YQ="
          },
          "xfr_mnemonic": "detect kit dignity giant coyote acid general child cart poet mystery cycle trumpet tent item gap cable boost finger vocal february dizzy embark library",
          "xfr_wallet_addr": "fra1r8emffn3v9ungs93axtqkcmxenksp6qwfqw2jarkfc4dptxgkreqfccezg"
        },
        {
          "tendermint_addr": "9E6F4C869F58795CFC5E35B8782FBFD8198D167B",
          "tendermint_pubkey": "LgiZAPZ2+Oayw1mf0nXJmMFAgw0pe7A6qzVOqeF7V/o=",
          "xfr_keypair": {
            "pub_key": "mM5X24xFFLiH454azJjmsynmcA1WIkJmwigta05D_wk=",
            "sec_key": "PTSw1hUngKIFBKC-soxqE1I5pZKb4OY0apeVK9KeuyQ="
          },
          "xfr_mnemonic": "flash genuine analyst poem ceiling click industry evolve wrestle walnut group future pull weapon ring exact misery olive suspect mention smooth fringe vault enhance",
          "xfr_wallet_addr": "fra1nr890kuvg52t3plrncdvex8xkv57vuqd2c3yyekz9qkkknjrluys3c3s88"
        },
        {
          "tendermint_addr": "B5D1AF330440FCE312048EF6CA33A9553C4265AE",
          "tendermint_pubkey": "8kYdjNcAFI0abbVzKywfLzX7y3sd/jDalQaZiz2z3KY=",
          "xfr_keypair": {
            "pub_key": "nO2ju_Wc_3F_n-evY8DBGAB-7LK-rDVmRur2qgD3pyw=",
            "sec_key": "AoivAz8x5Wh-PE4zZotuSSD0vt1cCYijbSRJeL7iQF0="
          },
          "xfr_mnemonic": "similar trick injury agree give stick shoot unknown glue bridge enroll check diagram grass patch organ early thank casual dolphin volume area acid recycle",
          "xfr_wallet_addr": "fra1nnk68wl4nnlhzlulu7hk8sxprqq8am9jh6kr2ejxatm25q8h5ukq6f34f9"
        }
      ]
    },
    "next_node_id": 7
  },
  "node_options_generator": null
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
# cat /tmp/__CHAIN_DEV__/ubuntu/bob/__DEV__/envs/MyEnv/3/mgmt.log

/tmp/tendermint-v0.33.8 node \
    --home /tmp/__CHAIN_DEV__/ubuntu/bob/__DEV__/envs/MyEnv/3 validator \
    validator \
    >>/tmp/__CHAIN_DEV__/ubuntu/bob/__DEV__/envs/MyEnv/3/tendermint.log 2>&1 &

EVM_CHAIN_ID=777 FINDORA_BLOCK_ITV=1 \
/tmp/abcid-v0.4.0-preview --enable-query-service --enable-eth-api-service \
    --tendermint-host 192.168.2.5 \
    --tendermint-port 41043 \
    --abcid-port 22480 \
    --submission-service-port 61919 \
    --ledger-service-port 61912 \
    --evm-http-port 21536 \
    --evm-ws-port 34983 \
    --ledger-dir /tmp/__CHAIN_DEV__/ubuntu/bob/__DEV__/envs/MyEnv/3/__findora__ \
    --tendermint-node-key-config-path /tmp/__CHAIN_DEV__/ubuntu/bob/__DEV__/envs/MyEnv/3/config/priv_validator_key.json \
    --disable-eth-empty-blocks \
    --checkpoint-file /tmp/checkpoint.toml \
    >>/tmp/__CHAIN_DEV__/ubuntu/bob/__DEV__/envs/MyEnv/3/app.log 2>&1 &
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

- `${CHAIN_DEV_GLOBAL_BASE_DIR}`
    - if defined, the base dir of all ENVs will be the value of this VAR
        - instead of the default value of `/tmp/__CHAIN_DEV__/$(hostname)/${USER}`

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

Let's check the inner structure of 'DEFAULT', `tree -F -L 1 /tmp/__CHAIN_DEV__/ubuntu/bob/__DEV__/envs/DEFAULT`:
```
/tmp/__CHAIN_DEV__/ubuntu/bob/__DEV__/envs/DEFAULT/
├── 0/           # seed node of this ENV, can *not* be removed dynamicly
├── 1/           # the first validator node of this ENV, can *not* be removed dynamicly
├── 2/           # the second validator node of this ENV, can be removed dynamicly
├── 3/           # the third validator node, similar with the second one
├── 4/           # ...
├── 5/           # ...
└── config.json  # config file of this ENV
```

Then further check the internal structure of a node, `tree -F -L 1 /tmp/__CHAIN_DEV__/ubuntu/bob/__DEV__/envs/DEFAULT/1`:

```
/tmp/__CHAIN_DEV__/ubuntu/bob/__DEV__/envs/DEFAULT/1/
├── config/              # config dir of the tendermint consensus engine
├── data/                # data dir of the tendermint consensus engine
├── tendermint.log       # log of the tendermint consensus engine
├── __findora__/         # data of the 'abcid' process
├── app.log              # log of the 'abcid' process
└── mgmt.log             # log of the 'fn dev' system
```

Inner management operations of `fn dev` will be logged in the `mgmt.log` file.

## Compatibility Notes

#### OS compatibility

In theory, it can run well on most Linux and MacOS versions, but it is not suitable for Windows operating systems, and there is currently no compatibility plan for Windows.
