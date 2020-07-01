FROM nexus.findora.org/zei:v0.0.3-1 as zei
FROM nexus.findora.org/rust:2020-05-15 as builder
RUN cargo install cargo-audit
RUN cargo install wasm-pack
RUN mkdir /app
WORKDIR /app/
COPY --from=zei /app /src/zei
COPY --from=zei /src/bulletproofs /src/bulletproofs
COPY . /app/
RUN cargo audit
RUN cargo build --release
RUN cargo test --no-fail-fast --release -j1 -- --ignored --test-threads=1
RUN cargo fmt -- --check
#Disabled because it triggers a compile and also tests dependencies
#RUN cargo clippy -- -D warnings
WORKDIR /app/components/wasm
RUN wasm-pack build --target nodejs
RUN bash -c 'time /app/target/release/log_tester /app/components/log_tester/example_log - /app/components/log_tester/expected'
#Cleanup some big files in release directory
RUN rm -r /app/target/release/build /app/target/release/deps

FROM debian:buster
COPY --from=builder /app/target/release /app
COPY --from=builder /app/components/wasm/pkg /app/wasm
WORKDIR /app/
CMD ls /app
