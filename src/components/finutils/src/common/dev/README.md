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
- Where can I get the FRA token?
- Where can I get the web-service ports of a target ENV?
- How to check the staking key of valiadtors?
- ...
In a word, how to easily get all necessary information?

Don't worry, a `fn dev show` will show you everything you need, you can use a shorter style `fn dev` when using the default cluster, they are equal.

Below is the information of a custom ENV named 'MyEnv', `fn dev -e MyEnv`:
```json
{
  "env_name": "MyEnv",
  "env_home_dir": "/tmp/__FINDORA_DEV__/envs/MyEnv",
  "tendermint_bin": "/tmp/tendermint-v0.33.8",
  "abcid_bin": "/tmp/abcid-v0.4.0-preview",
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
      "tendermint_addr": "27055305231CF6F718AD26485499B3ED1CCC8115",
      "tendermint_pubkey": "MVQUuYdJzxmYiJnmffxf40jBXXkwwnkLzt0f4XV+NeQ=",
      "xfr_keypair": {
        "pub_key": "5vy5t4sG_cbLVJvKQWn7xwd7WSmRQlUX6KW-IvDTLBo=",
        "sec_key": "fCBBCZCh8-IxCn9rgjoRiX03_SiuI99EouM-VJ9Hu5U="
      },
      "xfr_mnemonic": "chat fish hedgehog step help waste charge repeat cram public address visa around iron sponsor silly obvious sail squirrel relief arrive pet toe nominee",
      "xfr_wallet_addr": "fra1um7tndutqm7udj65n09yz60mcurhkkffj9p929lg5klz9uxn9sdqlv3f2e"
    },
    {
      "tendermint_addr": "33A24EF38E695529EED806CF70751C2CFC61DEAC",
      "tendermint_pubkey": "bvjpaBraL50Q8rRo+a7HH5HU2YI4lEasxm5CrP/TC6c=",
      "xfr_keypair": {
        "pub_key": "MOm22HEhE59rIb9H2Vgg_-Y0EOaIolKUbGcWXdfyZdM=",
        "sec_key": "AVj4I7G85a_dpETRjZbEYAlAiP7kBGxUx-3YBmYfhDc="
      },
      "xfr_mnemonic": "inject clerk accuse crunch absurd piece rely property bird win cave occur panther shaft switch device ketchup amused rather tragic settle celery dose brick",
      "xfr_wallet_addr": "fra1xr5mdkr3yyfe76epharajkpqllnrgy8x3z3999rvvut9m4ljvhfsjf5375"
    },
    {
      "tendermint_addr": "38AD0AF0258FB751262BD9D835A4D7848B0BC60E",
      "tendermint_pubkey": "5UzQOmtN9xT7kOLq9w0B4UA1U5dUeuJgzItbGkdepvg=",
      "xfr_keypair": {
        "pub_key": "zH51NYSEam0rA8aJTFlyxHY-t3KFAg9uTnZ47l0HTCI=",
        "sec_key": "S-WS-_maS1beU8fKBpTKBXUzZ8i1-9q44MlXYFiddak="
      },
      "xfr_mnemonic": "repeat noise rare rely open crucial awkward firm distance about detect connect style device wagon level check grow bench year toddler rubber sense advice",
      "xfr_wallet_addr": "fra1e3l82dvys34x62crc6y5cktjc3mradmjs5pq7mjwweuwuhg8fs3qpwjk0r"
    },
    {
      "tendermint_addr": "4F850CA6849EC0B3FC808D912F35CE2DAA222006",
      "tendermint_pubkey": "l+7AqB4df3ckzpF+7Ku4xh9z+WPzhyzjw99WKE0K9oI=",
      "xfr_keypair": {
        "pub_key": "RyqsOAAqHeEGW2AKwywDX7GgOTP5XjMfAnYlufOKmIM=",
        "sec_key": "CvaTBun0MiEeSErqHMPKRih0ADIu_UUjzuFQ2zdekuw="
      },
      "xfr_mnemonic": "harbor all speed train adult alien box steel promote discover desk antique please ship total carry coral deer method nominee blanket ghost brand have",
      "xfr_wallet_addr": "fra1gu42cwqq9gw7zpjmvq9vxtqrt7c6qwfnl90rx8czwcjmnuu2nzpsmg9205"
    },
    {
      "tendermint_addr": "C81783A1C67AD00C82FF093CE36AFF938185E44B",
      "tendermint_pubkey": "QPh4cyJnFKpDwDbU0C30hoQfqwvjCN9xDB8OtomhMkE=",
      "xfr_keypair": {
        "pub_key": "THNbrDo9hoWDCJKePWhFG3KWDhK7Hd76-envNmOLmOM=",
        "sec_key": "inIf4M_Pr_9O845CCp_BvMakx-lnjFaNPTvRUMSFv3E="
      },
      "xfr_mnemonic": "cricket know material crop wheel merge imitate script sing pioneer honey safe access inquiry wall best sign seed hybrid helmet wheat grocery stone west",
      "xfr_wallet_addr": "fra1f3e4htp68krgtqcgj20r66z9rdefvrsjhvwaa7hea8hnvcutnr3spzuwm3"
    },
    {
      "tendermint_addr": "FAD1E8BBC00835B88FAA41B54EAA0D2FCE55FEF0",
      "tendermint_pubkey": "i3QFHggYCqG6ciKmsjAXSt6NKfyUOGjfje5YwYaYOfo=",
      "xfr_keypair": {
        "pub_key": "UtKUdsJTYLCbmAXUAWGb97zMyW0HvyADxsrz7T4vmG0=",
        "sec_key": "o9aYwKN27PC3OPa7GqDpvP5q5WPoJk-qjRp4KTgNeok="
      },
      "xfr_mnemonic": "fever cheap upper flash team language town sudden cash nurse orphan dove nation possible bracket coil hello orchard hurdle feature excuse rigid flee basic",
      "xfr_wallet_addr": "fra12tffgakz2dstpxucqh2qzcvm777vejtdq7ljqq7xete76030npks7w80ku"
    }
  ],
  "block_interval": 1,
  "evm_chain_id": 777,
  "checkpoint_file": "/tmp/checkpoint.toml",
  "abcid_extra_flags": "--disable-eth-empty-blocks",
  "seed_nodes": {
    "0": {
      "id": 0,
      "tendermint_node_id": "91a4fa7b110d051bf9bc832622afb41d97f27997",
      "home_dir": "/tmp/__FINDORA_DEV__/envs/MyEnv/0",
      "kind": "Seed",
      "occupied_ports": {
        "web3_http_service": 27411,
        "web3_websocket_service": 24958,
        "tendermint_p2p_service": 32670,
        "tendermint_rpc_service": 56727,
        "abcid_abci_service": 56761,
        "abcid_submission_service": 20372,
        "abcid_ledger_query_service": 30015
      }
    }
  },
  "validator_or_full_nodes": {
    "1": {
      "id": 1,
      "tendermint_node_id": "4d4cdfb807697b680bffcf8cfbb6f90c43d60884",
      "home_dir": "/tmp/__FINDORA_DEV__/envs/MyEnv/1",
      "kind": "ValidatorOrFull",
      "occupied_ports": {
        "web3_http_service": 29062,
        "web3_websocket_service": 34909,
        "tendermint_p2p_service": 46897,
        "tendermint_rpc_service": 30160,
        "abcid_abci_service": 40160,
        "abcid_submission_service": 58302,
        "abcid_ledger_query_service": 38985
      }
    },
    "2": {
      "id": 2,
      "tendermint_node_id": "4b66d6e9e8e8e9277afda3024eda5474690f68aa",
      "home_dir": "/tmp/__FINDORA_DEV__/envs/MyEnv/2",
      "kind": "ValidatorOrFull",
      "occupied_ports": {
        "web3_http_service": 28888,
        "web3_websocket_service": 31504,
        "tendermint_p2p_service": 34962,
        "tendermint_rpc_service": 25973,
        "abcid_abci_service": 37529,
        "abcid_submission_service": 39327,
        "abcid_ledger_query_service": 33283
      }
    },
    "3": {
      "id": 3,
      "tendermint_node_id": "ef3e3dddeb5955efc8ef4905184008a28e4482b7",
      "home_dir": "/tmp/__FINDORA_DEV__/envs/MyEnv/3",
      "kind": "ValidatorOrFull",
      "occupied_ports": {
        "web3_http_service": 46711,
        "web3_websocket_service": 31297,
        "tendermint_p2p_service": 56883,
        "tendermint_rpc_service": 52160,
        "abcid_abci_service": 39108,
        "abcid_submission_service": 26479,
        "abcid_ledger_query_service": 56173
      }
    },
    "4": {
      "id": 4,
      "tendermint_node_id": "cc55016ba689320bbdb95d5922fc14d02716119e",
      "home_dir": "/tmp/__FINDORA_DEV__/envs/MyEnv/4",
      "kind": "ValidatorOrFull",
      "occupied_ports": {
        "web3_http_service": 29555,
        "web3_websocket_service": 34764,
        "tendermint_p2p_service": 28830,
        "tendermint_rpc_service": 33124,
        "abcid_abci_service": 22525,
        "abcid_submission_service": 26205,
        "abcid_ledger_query_service": 25639
      }
    },
    "5": {
      "id": 5,
      "tendermint_node_id": "ba1cdbbf61b59520f01ee6769777edc0a4af8a07",
      "home_dir": "/tmp/__FINDORA_DEV__/envs/MyEnv/5",
      "kind": "ValidatorOrFull",
      "occupied_ports": {
        "web3_http_service": 55174,
        "web3_websocket_service": 57460,
        "tendermint_p2p_service": 47144,
        "tendermint_rpc_service": 48775,
        "abcid_abci_service": 29531,
        "abcid_submission_service": 46879,
        "abcid_ledger_query_service": 49426
      }
    },
    "6": {
      "id": 6,
      "tendermint_node_id": "bda665baeb0a82c8e9892358fe082dd1ead5ff2a",
      "home_dir": "/tmp/__FINDORA_DEV__/envs/MyEnv/6",
      "kind": "ValidatorOrFull",
      "occupied_ports": {
        "web3_http_service": 46340,
        "web3_websocket_service": 64310,
        "tendermint_p2p_service": 24224,
        "tendermint_rpc_service": 23894,
        "abcid_abci_service": 41878,
        "abcid_submission_service": 50118,
        "abcid_ledger_query_service": 31215
      }
    }
  },
  "tendermint_genesis_config": "{\"app_hash\":\"\",\"chain_id\":\"test-chain-wfnI4D\",\"consensus_params\":{\"block\":{\"max_bytes\":\"22020096\",\"max_gas\":\"-1\",\"time_iota_ms\":\"1000\"},\"evidence\":{\"max_age_duration\":\"172800000000000\",\"max_age_num_blocks\":\"100000\"},\"validator\":{\"pub_key_types\":[\"ed25519\"]}},\"genesis_time\":\"2022-09-06T01:25:39.871204204Z\",\"validators\":[{\"address\":\"38AD0AF0258FB751262BD9D835A4D7848B0BC60E\",\"name\":\"node-0\",\"power\":\"1\",\"pub_key\":{\"type\":\"tendermint/PubKeyEd25519\",\"value\":\"5UzQOmtN9xT7kOLq9w0B4UA1U5dUeuJgzItbGkdepvg=\"},\"voting_power\":\"1\"},{\"address\":\"27055305231CF6F718AD26485499B3ED1CCC8115\",\"name\":\"node-1\",\"power\":\"1\",\"pub_key\":{\"type\":\"tendermint/PubKeyEd25519\",\"value\":\"MVQUuYdJzxmYiJnmffxf40jBXXkwwnkLzt0f4XV+NeQ=\"},\"voting_power\":\"1\"},{\"address\":\"FAD1E8BBC00835B88FAA41B54EAA0D2FCE55FEF0\",\"name\":\"node-2\",\"power\":\"1\",\"pub_key\":{\"type\":\"tendermint/PubKeyEd25519\",\"value\":\"i3QFHggYCqG6ciKmsjAXSt6NKfyUOGjfje5YwYaYOfo=\"},\"voting_power\":\"1\"},{\"address\":\"4F850CA6849EC0B3FC808D912F35CE2DAA222006\",\"name\":\"node-3\",\"power\":\"1\",\"pub_key\":{\"type\":\"tendermint/PubKeyEd25519\",\"value\":\"l+7AqB4df3ckzpF+7Ku4xh9z+WPzhyzjw99WKE0K9oI=\"},\"voting_power\":\"1\"},{\"address\":\"33A24EF38E695529EED806CF70751C2CFC61DEAC\",\"name\":\"node-4\",\"power\":\"1\",\"pub_key\":{\"type\":\"tendermint/PubKeyEd25519\",\"value\":\"bvjpaBraL50Q8rRo+a7HH5HU2YI4lEasxm5CrP/TC6c=\"},\"voting_power\":\"1\"},{\"address\":\"C81783A1C67AD00C82FF093CE36AFF938185E44B\",\"name\":\"node-5\",\"power\":\"1\",\"pub_key\":{\"type\":\"tendermint/PubKeyEd25519\",\"value\":\"QPh4cyJnFKpDwDbU0C30hoQfqwvjCN9xDB8OtomhMkE=\"},\"voting_power\":\"1\"}]}",
  "next_node_id": 7
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
    fn dev create [OPTIONS]

FLAGS:
    -h, --help       Prints help information
    -V, --version    Prints version information

OPTIONS:
    -A, --abcid-bin-path <ABCID BIN PATH>              The path of your custom abcid binary
    -E, --abcid-extra-flags <ABCID EXTRA FLAGS>        A pair of quotes should be used when specifying extra flags
    -i, --block-itv-secs <BLOCK INTERVAL>              Block interval in seconds
    -c, --checkpoint-file <CHECKPOINT FILE>            The file path of the checkpoint file
    -e, --env-name <ENV NAME>                          The name of the target env
    -I, --evm-chain-id <EVM CHAIN ID>                  The chain id in the scope of evm logic
    -H, --host-ip <HOST IP>                            The IP of your local host, default to 127.0.0.1
    -T, --tendermint-bin-path <TENDERMINT BIN PATH>    The path of your custom tendermint binary
    -N, --validator-num <VALIDATOR NUMBER>             How many initial validators should be created
```

For the issue of remote joint debugging, we can use the `--host-ip` option to specify the listening address of the target ENV.

A few other commonly used options:
- `-i, --block-itv-secs`, block interval, default to 3s
- `-c, --checkpoint-file`, the path of you custom checkpoint file
- `-I, --evm-chain-id`, the value of this option will be defined as `${EVM_CHAIN_ID}`
- `-N, --validator-num`, 5 initial validators will be created by default, you can change the number by this option
- `-T, --tendermint-bin-path`, use a custom version of the tendermint binary
- `-A, --abcid-bin-path`, use a custom versioln of the abcid binary
- `-E, --abcid-extra-flags`, specify extra flags
  - for example, there is a `--disable-eth-empty-blocks` flag in the `abcid` binary, and it will not be set by `fn dev` by default, you can set it like this `fn dev create -E '--disable-eth-empty-blocks'`

Below is a more complete example with richer options:
```shell
fn dev create \
    -H 192.168.2.5 \
    -e MyEnv \
    -i 1 \
    -N 6 \
    -I 777 \
    -c /tmp/checkpoint.toml \
    -T /tmp/tendermint-v0.33.8 \
    -A /tmp/abcid-v0.4.0-preview \
    -E '--disable-eth-empty-blocks'
```

- all nodes of this ENV will listen on '192.168.2.5'
  - now you can tell the IP address to your frond-end engineers, the joint debugging will be ok
- the name of this ENV is 'MyEnv'
- the block interval will be 1s
- the number of initial validator nodes is 6
- the chain id of evm is 777
- the path of checkpoint file is '/tmp/checkpoint.toml'
- use custom binaries of tendermint and abcid
- `--disable-eth-empty-blocks` is added as an extra flag

The coresponding starting commands of nodes will be(but in one-line style):
```shell
# cat /tmp/__FINDORA_DEV__/envs/MyEnv/3/fn_dev.log

/tmp/tendermint-v0.33.8 node \
    --home /tmp/__FINDORA_DEV__/envs/MyEnv/3 \
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
