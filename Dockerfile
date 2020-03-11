FROM nexus.findora.org/zei:v0.0.1 as zei
FROM rustlang/rust:nightly as builder
RUN cargo install cargo-audit
RUN cargo install wasm-pack
RUN mkdir /app
WORKDIR /app/
COPY --from=zei /app /src/zei
COPY --from=zei /src/zcash-bn-fork /src/zcash-bn-fork
COPY . /app/
RUN cargo audit
RUN cargo test --no-run
RUN cargo test --no-fail-fast --workspace --exclude 'txn_builder_cli'
RUN cargo build --release
WORKDIR /app/components/wasm
RUN wasm-pack build

FROM debian:buster
COPY --from=builder /app/target/release /app
COPY --from=builder /app/components/wasm/pkg /app/wasm
WORKDIR /app/
CMD ls /app
