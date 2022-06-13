FROM musl_fn_macos_base
ENV WORK_DIR /volume
COPY . $WORK_DIR
WORKDIR $WORK_DIR
ENV OPENSSL_DIR /musl
RUN rustup target add x86_64-apple-darwin
ENV PATH="/opt/osxcross/target/bin:$PATH"
ENV CC=o64-clang
ENV CXX=o64-clang++
RUN cargo build -p finutils --release --target x86_64-apple-darwin

CMD ["sleep", "999999"]