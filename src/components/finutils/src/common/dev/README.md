# fn dev

## User guide

**TODO**

## Compatibility Notes

#### No breaks to musl compilation

```shell
# host: Linux 5.15.52-gentoo #2 SMP Tue Jul 26 15:14:31 CST 2022 x86_64  GNU/Linux

X86_64_UNKNOWN_LINUX_MUSL_OPENSSL_LIB_DIR=/usr/x86_64-unknown-linux-musl/usr/lib \
X86_64_UNKNOWN_LINUX_MUSL_OPENSSL_INCLUDE_DIR=/usr/x86_64-unknown-linux-musl/usr/include \
\
cargo build --target=x86_64-unknown-linux-musl -p finutils
```
