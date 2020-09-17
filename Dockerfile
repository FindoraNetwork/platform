FROM 563536162678.dkr.ecr.us-west-2.amazonaws.com/zei:v0.0.3-5 as zei
FROM 563536162678.dkr.ecr.us-west-2.amazonaws.com/rust:2020-05-15 as builder
RUN apt-get update
RUN apt-get install -y bats parallel
ENV RUSTC_WRAPPER='/usr/local/cargo/bin/sccache'
ENV SCCACHE_REDIS='redis://redis/'
RUN cargo install cargo-deb
RUN cargo install cargo-audit
RUN cargo install wasm-pack
RUN mkdir /app
WORKDIR /app/
COPY --from=zei /app /src/zei
COPY --from=zei /src/bulletproofs /src/bulletproofs
COPY . /app/
RUN cargo audit
RUN cargo build --release
RUN cargo test --release --no-run
RUN { echo 'bash components/cli2/run_tests_local.sh'; cargo test --release -- --list 2>&1 >/dev/null  | sed -n 's/^\s*Running \(\S*\)\s*$/\1\n\1 --ignored/p'; } | FINDORA_TXN_CLI_DATA_SEARCH_PATH=`pwd`/components/txn_cli FINDORA___TEST___PROJECT___ROOT=`pwd` parallel {}
# RUN cargo test --no-fail-fast --release -- --report-time
# RUN cargo test --no-fail-fast --release -- --ignored --report-time
RUN cargo fmt -- --check
RUN cargo deb -p cli2
#Disabled because it triggers a compile and also tests dependencies
#RUN cargo clippy -- -D warnings
WORKDIR /app/components/wasm
RUN wasm-pack build --target nodejs
#Cleanup some big files in release directory
RUN rm -r /app/target/release/build /app/target/release/deps


FROM debian:buster
COPY --from=builder /app/target/release /app
COPY --from=builder /app/target/debian /app/debian
COPY --from=builder /app/components/wasm/pkg /app/wasm
WORKDIR /app/
CMD ls /app
