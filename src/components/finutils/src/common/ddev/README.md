# `fn ddev`

This is a distributed version of [`fn dev`](../dev/README.md).

## User guide

#### Quick start

Through a `fn ddev -h` we can see:

```
fn-ddev
Manage development clusters on remote hosts

USAGE:
    fn ddev [OPTIONS] [SUBCOMMAND]

FLAGS:
    -h, --help       Prints help information
    -V, --version    Prints version information

OPTIONS:
    -e, --env-name <ENV NAME>    The name of the target env

SUBCOMMANDS:
    create               Create a new env
    destroy              Destroy an existing env
    destroy-all          Destroy all existing ENVs
    help                 Prints this message or the help of the given subcommand(s)
    host-exec            Execute commands on all remote hosts
    host-get-file        Get a remote file from all remote hosts
    host-put-file        Put a local file to all remote hosts
    init                 Config the initial settings(POS,FRA issuance...)
    init-all             Apply the `init` operation to all existing ENVs
    list                 List the names of all existing ENVs
    node-collect-logs    Collect all node logs from remote hosts to local host
    pop-node             Pop a node from an existing env
    push-node            Attach a new node to an existing env
    show                 Default operation, show the information of an existing env
    show-all             Show the details of all existing ENVs
    start                Start an existing env
    start-all            Start all existing ENVs
    stop                 Stop an existing env
    stop-all             Stop all existing ENVs
```

Compile binaries on your local machine:
```shell
# get the source code
git clone https://github.com/FindoraNetwork/platform.git
cd platform

# toolchains of 'rust' and 'go'('make', 'perl', 'jq' ... are also recommended),
# should be installed first if they are not ready;
make
```

Set the ssh public key(eg `~/.ssh/id_rsa.pub`) of your localhost to the correct path(eg `~/.ssh/authorized_keys`) on every remote host, and then compile binaries on all remote hosts:
```shell
# This is just an example,
# please `export` according to your actual remote hosts
export FN_DDEV_HOSTS='10.0.0.3#ubuntu,10.0.0.4#ubuntu,10.0.0.5#ubuntu'
fn ddev host-exec --cmd \
    'git clone https://github.com/FindoraNetwork/platform.git && cd platform && make'
```

The above command may fail if the remote host lacks some necessary dependencies, if this is the case, it is also possible to batch process remote hosts on your local machine using `fn ddev host-exec`:
```shell
# This is just an example,
# please install according to your actual missing dependencies
fn ddev host-exec --cmd \
    "sudo su -c 'apt install -y make perl jq libssl-dev && snap install go --classic'"
```

> NOTE: If you don't like the batch mode of  `fn ddev host-exec`, you can of course install all the dependencies manually on a host-by-host basis.

Assume your have 3 remote hosts,
and you have set the ssh public key of your local machine on each of them:
- `10.0.0.2#alice`
- `10.0.0.3#bob`
- `10.0.0.4#jack`

Create and start a distributed cluster:
```shell
# this distributed cluster has 4 validator nodes and 1 seed node
fn ddev create --hosts '10.0.0.2#alice,10.0.0.3#bob,10.0.0.4#jack'
```

If all the user names are same as the user name of your local machine, the above can be simplified to:
```shell
fn ddev create --hosts '10.0.0.2,10.0.0.3,10.0.0.4'
```

Web3 endpoints:
- `http://${web3_host}:${http_port}'`
    - `web3_host=$(fn ddev | jq '.meta.validator_or_full_nodes."1".host."addr"')`
    - `http_port=$(fn ddev | jq '.meta.validator_or_full_nodes."1".ports."web3_http_service")`
- `http://${web3_host}:${ws_port}'`
    - `web3_host=$(fn ddev | jq '.meta.validator_or_full_nodes."1".host."addr"')`
    - `ws_port=$(fn ddev | jq '.meta.validator_or_full_nodes."1".ports."web3_websocket_service")`

#### Management of 'a single cluster/multiple clusters'

The usage of this section is almost the same as `fn dev`, except that you must specify an additional `--hosts` option to define the necessary information for all remote hosts.

There is also an environment variable named `$FN_DDEV_HOSTS` that has the same function as this option, but this option has a higher priority.

The format of acceptable values for this option is as follows:
- `host_ip#remote_user#ssh_port#host_weight#ssh_local_private_key,...`
    - example: `10.0.0.2#bob#22#1#/home/bob/.ssh/id_rsa`
- `host_ip#remote_user#ssh_port#host_weight,...`
    - example: `10.0.0.2#bob#22#9,10.0.0.3#bob#22#5`
    - this style omitted the `ssh_local_private_key` field, its value will be `$HOME/.ssh/id_rsa`
- `host_ip#remote_user#ssh_port,...`
    - example: `10.0.0.2#bob#22,10.0.0.3#bob#22`
    - this style further omitted the `host_weight` field, its value will be automatically calculated according to the number of CPUs and single computing power of each host
- `host_ip#remote_user,...`
    - example: `10.0.0.2#bob,10.0.0.3#bob`
    - this style further omitted the `ssh_port` field, its value will be '22'
- `host_ip,...`
    - example: `10.0.0.2,10.0.0.3`
    - this style further omitted the `remote_user` field, its value will be `$USER` of your local machine

NOTE:
- the delimiter between hosts is ','
- the delimiter between fields is '#'
- if there are some whitespace characters in the content, they will be trimed automatically

Also, there are additional 4 options:
- `--host-put-file`, put a local file to all remote hosts
- `--host-get-file`, get a remote file from all remote hosts
- `--host-exec`, execute commands on all remote hosts
- `--collect-node-logs`, collect all node logs from remote hosts to local host

#### Environment variable definitions

- `${FN_DDEV_HOSTS}`
    - if defined, you need not to set the `--hosts` option
    - if you set the `--hosts` at the same time, the value of this VAR will be ignored
- `${CHAIN_DEV_GLOBAL_BASE_DIR}`
    - if defined, the base dir of all ENVs will be the value of this VAR
        - instead of the default value of `/tmp/__CHAIN_DEV__/$(hostname)/${USER}`

#### Internal organization of data and logs

All data and logs are located under the `/tmp/__CHAIN_DEV__/ubuntu/bob/__D_DEV__` of each host, so you should have a big enough `/tmp`.

We can use `tree -F -L 2 /tmp/__CHAIN_DEV__/ubuntu/bob/__D_DEV__` to check their structures:
```
/tmp/__CHAIN_DEV__/ubuntu/bob/__D_DEV__/
├── envs/               # existing ENVs
│   ├── DEFAULT/        # the default ENV
│   ├── env_A/          # a custom ENV named 'env_A'
│   └── env_B/          # a custom ENV named 'env_B'
└── ports_cache         # allocated ports
```

Let's check the inner structure of 'DEFAULT', `tree -F -L 1 /tmp/__CHAIN_DEV__/ubuntu/bob/__D_DEV__/envs/DEFAULT`:
```
/tmp/__CHAIN_DEV__/ubuntu/bob/__D_DEV__/envs/DEFAULT/
├── 0/           # seed node of this ENV, can *not* be removed dynamicly
├── 1/           # the first validator node of this ENV, can *not* be removed dynamicly
├── 2/           # the second validator node of this ENV, can be removed dynamicly
├── 8/           # ...
└── config.json  # config file of this ENV
```

Then further check the internal structure of a node, `tree -F -L 1 /tmp/__CHAIN_DEV__/ubuntu/bob/__D_DEV__/envs/DEFAULT/1`:

```
/tmp/__CHAIN_DEV__/ubuntu/bob/__D_DEV__/envs/DEFAULT/1/
├── config/              # config dir of the tendermint consensus engine
├── data/                # data dir of the tendermint consensus engine
├── tendermint.log       # log of the tendermint consensus engine
├── __findora__/         # data produced by the 'abcid' process
├── app.log              # log of the 'abcid' process
└── mgmt.log             # log of the 'fn ddev' system
```

Inner management operations of `fn ddev` will be logged in the `mgmt.log` file.

## Compatibility Notes

#### OS compatibility

In theory, it can run well on most Linux distributions and MacOS versions, but it is not suitable for Windows operating systems, and there is currently no compatibility plan for Windows.
