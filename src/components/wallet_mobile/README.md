# wallet-mobile-libs
Compiled by [platform/componets/wallet_mobile](https://github.com/FindoraNetwork/platform/tree/wallet_mobile/components/wallet_mobile)

## Usage

### Android example
- https://medium.com/visly/rust-on-android-19f34a2fb43
- [Chinese Example](https://zhuanlan.zhihu.com/p/73473362)

### IOS example
- https://medium.com/visly/rust-on-ios-39f799b3c1dd
- [Chinese Example](https://zhuanlan.zhihu.com/p/73890910)

## Compile

### Wasm
```
cd components/wallet_mobile
wasm-pack build
```

### Android
```
export TARGET_AR=~/.NDK/arm64/bin/aarch64-linux-android-ar
export TARGET_CC=~/.NDK/arm64/bin/aarch64-linux-android-clang

cargo build -p wallet_mobile --target aarch64-linux-android --release --lib
```

### IOS
```
cargo lipo --release -p wallet_mobile
```


