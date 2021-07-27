# Findora Platform

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

> For MacOS
>
> - Install XCode
> - Install Developer tools
>
> ```shell
> # install rust
> curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
>
> # install wasm-pack
> curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
>
> # install homebrew
> /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
>
> # install golang
> brew install golang
>
> # install system-deps
> brew install gawk glibc openssl wget leveldb
>
> # create directory for binaries
> mkdir ~/go/bin
> ```

#### install toml CLI

`pip3 install toml-cli`

#### ssh git login

`cargo` needs a ssh login to fetch git repos. Generate a ssh key if you don't have one already  with `ssh-keygen` and it to your
Github account at `https://github.com/settings/keys`

#### compile

dynamic linked for online env(docker):

```
$ make
$ tree -F release

release
├── bin
│   ├── abci_validator_node
│   ├── findora
│   ├── query_server
│   └── tendermint
└── lib
    └── wasm.tar.gz
```

static linked binary, that can run on most linux hosts, for development env:

```
$ make build_release_musl_debug
$ tree -F release

release
├── bin
│   ├── abci_validator_node
│   ├── findora
│   ├── query_server
│   └── tendermint
└── lib
    └── wasm.tar.gz
```
