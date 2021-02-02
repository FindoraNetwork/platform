# Findora Platform

Top level targets: executable and plug-in components of the Findora platform
* Application library for performing validation and maintaining a ledger
* Binding executables/shared libraries for compatible consensus layers (e.x. Tendermint ABCI binding execuatable)
* Command line executable for generating transactions, managing local cryptographic keys, etc.
* WASM interface (exposes the same functionality as CLI, for single page browser apps).
* any future executable and distributable components

```
+
+-- core/ - shared libraries providing common application support components
|    +-- data_model/ - common types
|    +== store/ = common resource lookup & retrieval
|    +== <module1>/
|    | ...
|    +== <moduleK>/
|
+-- components/ - capabilities that can be incorporated into one or more top level targets
|    +-- ledger_app/ - provides a ledger interface with transaction validation and post-transaction state updates.
|    +== <component1>/
|    | ...
|    +-- <componentM>
|
+-- <target1>/
+-- <target2>/
| ...
+-- <targetN>/

```

## build

#### prepare

Assume your OS is ubuntu(1804 or 2004):

```shell
# install Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# install wasm-pack
curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh

# install golang
cd /opt && \
    sudo wget https://golang.google.cn/dl/go1.15.7.linux-amd64.tar.gz && \
    sudo tar -xpf go1.15.7.linux-amd64.tar.gz && \
    echo "export PATH=/opt/go/bin:$PATH" >> /etc/profile && \
    source /etc/profile

# install system-deps
sudo apt install libc-dev libssl-dev make git curl wget
```

#### compile

```
$ make
$ tree -F ./release

./release
|-- bin/
|   |-- abci_validator_node*
|   |-- check_merkle
|   |-- findora
|   |-- query_server*
|   |-- solvency_cli
|   |-- tendermint*
|   `-- txn_cli
`-- lib/
    `-- wasm.tar.gz
```

```
$ make build DBG=1
$ tree -F ./debug

./debug
|-- bin/
|   |-- abci_validator_node*
|   |-- check_merkle
|   |-- findora
|   |-- query_server*
|   |-- solvency_cli
|   |-- tendermint*
|   `-- txn_cli
`-- lib/
    `-- wasm.tar.gz
```
