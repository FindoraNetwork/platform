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
    add-node       Attach a new node to an existing env
    create         Create a new env
    del-node       Pop a node from an existing env
    destroy        Destroy an existing env
    destroy-all    Destroy all existing ENVs
    help           Prints this message or the help of the given subcommand(s)
    info           Default operation, show the information of an existing env
    info-all       Show the details of all existing ENVs
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

But wait, where can I get the FRA token? How to check the staking key of valiadtors? In a word, how to easily view these necessary information?

Don't worry, a `fn dev info` will show you everything you need, you can use a shorter style `fn dev` when using the default cluster, they are equal.

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
  "initial_pos_settings": [],
  "block_interval": 1,
  "evm_chain_id": 777,
  "checkpoint_file": "/tmp/checkpoint.toml",
  "abcid_extra_flags": "--disable-eth-empty-blocks",
  "seed_nodes": {
    "0": {
      "id": 0,
      "tm_id": "7caec7b3a25c3966c6603452c9095a1e2ff19785",
      "home": "/tmp/__FINDORA_DEV__/envs/MyEnv/0",
      "kind": "Seed",
      "ports": {
        "web3_http": 44752,
        "web3_ws": 30047,
        "tm_p2p": 37377,
        "tm_rpc": 23863,
        "app_abci": 42327,
        "app_8669": 39832,
        "app_8668": 45102
      }
    }
  },
  "validator_or_full_nodes": {
    "1": {
      "id": 1,
      "tm_id": "bb81551e737b232285ecd563c499ba56a9335ed8",
      "home": "/tmp/__FINDORA_DEV__/envs/MyEnv/1",
      "kind": "Node",
      "ports": {
        "web3_http": 39118,
        "web3_ws": 38781,
        "tm_p2p": 43052,
        "tm_rpc": 52559,
        "app_abci": 32767,
        "app_8669": 45090,
        "app_8668": 58272
      }
    },
    "2": {
      "id": 2,
      "tm_id": "25af74b05bd5a312f64ef2155ad2844e87cbf1a4",
      "home": "/tmp/__FINDORA_DEV__/envs/MyEnv/2",
      "kind": "Node",
      "ports": {
        "web3_http": 30247,
        "web3_ws": 30301,
        "tm_p2p": 62187,
        "tm_rpc": 34517,
        "app_abci": 59392,
        "app_8669": 39727,
        "app_8668": 42059
      }
    },
    "3": {
      "id": 3,
      "tm_id": "03eac7c59230e345cfc0a030dbd2c1bad003c75c",
      "home": "/tmp/__FINDORA_DEV__/envs/MyEnv/3",
      "kind": "Node",
      "ports": {
        "web3_http": 62442,
        "web3_ws": 34670,
        "tm_p2p": 33516,
        "tm_rpc": 40108,
        "app_abci": 62630,
        "app_8669": 61792,
        "app_8668": 41703
      }
    },
    "4": {
      "id": 4,
      "tm_id": "0892a3c97b664ea35c6f7815b0dc20699a0b2a13",
      "home": "/tmp/__FINDORA_DEV__/envs/MyEnv/4",
      "kind": "Node",
      "ports": {
        "web3_http": 25335,
        "web3_ws": 30296,
        "tm_p2p": 21710,
        "tm_rpc": 49938,
        "app_abci": 33458,
        "app_8669": 42395,
        "app_8668": 23640
      }
    },
    "5": {
      "id": 5,
      "tm_id": "0a184e1f6faddb86cbf42940a739345965021dbe",
      "home": "/tmp/__FINDORA_DEV__/envs/MyEnv/5",
      "kind": "Node",
      "ports": {
        "web3_http": 38777,
        "web3_ws": 64178,
        "tm_p2p": 34144,
        "tm_rpc": 36390,
        "app_abci": 30303,
        "app_8669": 32079,
        "app_8668": 24219
      }
    },
    "6": {
      "id": 6,
      "tm_id": "7bf456af4aa2ae6e933876d2da62a5e6109a141e",
      "home": "/tmp/__FINDORA_DEV__/envs/MyEnv/6",
      "kind": "Node",
      "ports": {
        "web3_http": 22716,
        "web3_ws": 22631,
        "tm_p2p": 47611,
        "tm_rpc": 57745,
        "app_abci": 26842,
        "app_8669": 40852,
        "app_8668": 54885
      }
    }
  },
  "tendermint_genesis_config": "{\"app_hash\":\"\",\"chain_id\":\"test-chain-zfWF7p\",\"consensus_params\":{\"block\":{\"max_bytes\":\"22020096\",\"max_gas\":\"-1\",\"time_iota_ms\":\"1000\"},\"evidence\":{\"max_age_duration\":\"172800000000000\",\"max_age_num_blocks\":\"100000\"},\"validator\":{\"pub_key_types\":[\"ed25519\"]}},\"genesis_time\":\"2022-09-05T18:11:48.125514813Z\",\"validators\":[{\"address\":\"1EC4D45D32369471CFC2D72BA13BBC763F8F7F01\",\"name\":\"node-0\",\"power\":\"1\",\"pub_key\":{\"type\":\"tendermint/PubKeyEd25519\",\"value\":\"1JLCxVB57o6O0kvKO4SqcxnVbbiyy8cOFJZ6VaXNPRE=\"},\"voting_power\":\"1\"},{\"address\":\"7860BD6D1E825C0BD64E58CAAF007028F22EB6EF\",\"name\":\"node-1\",\"power\":\"1\",\"pub_key\":{\"type\":\"tendermint/PubKeyEd25519\",\"value\":\"34JI8V8j/z4wT9MRtpZF3AtNdJxwt8guQfn4lhdnLUM=\"},\"voting_power\":\"1\"},{\"address\":\"AD1EE8FCCB2D69C2FD0E038DA431B0A0264B1F2D\",\"name\":\"node-2\",\"power\":\"1\",\"pub_key\":{\"type\":\"tendermint/PubKeyEd25519\",\"value\":\"ajDU9S6SMwkBtvIyJ838CuGDLq+MN6ZUFjWfRAY2rp4=\"},\"voting_power\":\"1\"},{\"address\":\"860FCA9E795E1982BEAC75F7DBF1A8E55D82749C\",\"name\":\"node-3\",\"power\":\"1\",\"pub_key\":{\"type\":\"tendermint/PubKeyEd25519\",\"value\":\"MJm6lkkwW2ZYd2Z1QTwAK5eLPvmzcFpYAi+PN4XthJ0=\"},\"voting_power\":\"1\"},{\"address\":\"19272E9A28AE77D81A2B2768F1C2B9EA75AC8670\",\"name\":\"node-4\",\"power\":\"1\",\"pub_key\":{\"type\":\"tendermint/PubKeyEd25519\",\"value\":\"3yyNXD503Fctiwmbw8JTk2jdZBDA2Drm9CkSw/VCTvo=\"},\"voting_power\":\"1\"},{\"address\":\"078E2E0F813C3B92032812E6E87A2D3262A55A2C\",\"name\":\"node-5\",\"power\":\"1\",\"pub_key\":{\"type\":\"tendermint/PubKeyEd25519\",\"value\":\"Ygg2h6SAsNSud/PsGSiJ86KjXdeeUa+dqiV4zGPcDok=\"},\"voting_power\":\"1\"}]}",
  "next_node_id": 7
}
```

You can pause the cluster by `fn dev stop`, and resume it by `fn dev start` at any time; you can also scale up the cluster by `fn dev add-node`, and scale it down by `fn dev del-node`.

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
- `fn dev info-all`, list the details of all existing ENVs
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
