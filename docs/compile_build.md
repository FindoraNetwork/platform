# build

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

# install toml CLI
`pip3 install toml-cli`
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
