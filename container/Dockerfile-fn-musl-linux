FROM clux/muslrust
ENV WORK_DIR /volume
COPY . $WORK_DIR
WORKDIR $WORK_DIR
ENV OPENSSL_DIR /musl
RUN rustup target add x86_64-unknown-linux-musl && cargo build --release --bins -p finutils --target=x86_64-unknown-linux-musl 

CMD ["sleep", "999999"]