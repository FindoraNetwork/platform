FROM 563536162678.dkr.ecr.us-west-2.amazonaws.com/zei:v0.0.3-5 as zei
FROM 563536162678.dkr.ecr.us-west-2.amazonaws.com/rust:2020-09-15 as builder
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
RUN { echo 'cargo audit'; echo 'cargo fmt -- --check'; } | parallel {}
RUN { echo 'cargo build --release'; echo 'cargo test --release --no-run'; } | parallel -j2 -u {}
RUN cp target/release/findora findora_cli
RUN { echo 'bash components/cli2/run_tests_local.sh'; echo 'cargo deb -p cli2'; cargo test --release -- --list 2>&1 >/dev/null  | sed -n 's/^\s*Running \(\S*\)\s*$/\1\n\1 --ignored/p'; } | CLI2=`pwd`/findora_cli FINDORA_TXN_CLI_DATA_SEARCH_PATH=`pwd`/components/txn_cli FINDORA___TEST___PROJECT___ROOT=`pwd` parallel {}
# RUN cargo test --no-fail-fast --release -- --report-time
# RUN cargo test --no-fail-fast --release -- --ignored --report-time
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
