#
#  The default target for this Makefile creates the "release"
#  subdirectory, which is a start at collecting the files that
#  will be needed for a release.
#
#  The "test_status" target performs the standard and release
#  builds, and then runs the test.  The final step is making
#  the release target.  If all that succeeds, the source has
#  some chance of being in reasonable shape.
#

all: build_release

export CARGO_NET_GIT_FETCH_WITH_CLI = true
export PROTOC = $(shell which protoc)

ifdef DBG
target_dir = debug
else
target_dir = release
endif

bin_dir         = bin
lib_dir         = lib
pick            = target/$(target_dir)
release_subdirs = $(bin_dir) $(lib_dir)

bin_files = \
		./$(pick)/findora \
		./$(pick)/abci_validator_node \
		./$(pick)/query_server \
		$(shell go env GOPATH)/bin/tendermint

bin_files_musl_debug = \
		./target/x86_64-unknown-linux-musl/$(target_dir)/findora \
		./target/x86_64-unknown-linux-musl/$(target_dir)/abci_validator_node \
		./target/x86_64-unknown-linux-musl/$(target_dir)/query_server \
		$(shell go env GOPATH)/bin/tendermint

WASM_PKG = wasm.tar.gz
lib_files = ./$(WASM_PKG)

define pack
	-@ rm -rf $(target_dir)
	mkdir $(target_dir)
	cd $(target_dir); for i in $(release_subdirs); do mkdir $$i; done
	cp $(bin_files) $(target_dir)/$(bin_dir)
	cp $(lib_files) $(target_dir)/$(lib_dir)
endef

define pack_musl_debug
	-@ rm -rf $(target_dir)
	mkdir $(target_dir)
	cd $(target_dir); for i in $(release_subdirs); do mkdir $$i; done
	cp $(bin_files_musl_debug) $(target_dir)/$(bin_dir)
	cp $(lib_files) $(target_dir)/$(lib_dir)
endef

build: tendermint wasm
ifdef DBG
	cargo build --frozen --bins -p abci_validator_node -p query_api -p cli2
	$(call pack,$(target_dir))
else
	@ echo -e "\x1b[31;01m\$$(DBG) must be defined !\x1b[00m"
	@ exit 1
endif

build_release: tendermint wasm
ifdef DBG
	@ echo -e "\x1b[31;01m\$$(DBG) must NOT be defined !\x1b[00m"
	@ exit 1
else
	cargo build --frozen --release --bins -p abci_validator_node -p query_api -p cli2
	$(call pack,$(target_dir))
endif

build_release_musl_debug: tendermint wasm
ifdef DBG
	@ echo -e "\x1b[31;01m\$$(DBG) must NOT be defined !\x1b[00m"
	@ exit 1
else
	cargo build --target=x86_64-unknown-linux-musl --features=debugenv --frozen --release --bins -p abci_validator_node -p query_api -p cli2
	$(call pack_musl_debug,$(target_dir))
endif

test:
	cargo test --release --workspace -- --test-threads=1
	cargo test --release --workspace -- --ignored

bench:
	cargo bench --workspace

lint:
	cargo clippy --workspace
	cargo clippy --workspace --tests

test_status:
	scripts/incur build
	scripts/incur build --release
	scripts/incur test
	make build_release

fmt:
	@ bash ./tools/fmt.sh

clean:
	@ cargo clean
	@ rm -rf debug release Cargo.lock

tendermint:
	if [ -d ".git" ]; then \
		git submodule update --init --recursive; \
	else \
		if [ -d "tools/tendermint" ]; then rm -rf tools/tendermint; fi; \
		git clone -b v0.33.5 --depth=1 https://github.com/tendermint/tendermint.git tools/tendermint; \
	fi
	cd tools/tendermint && make install

wasm:
	cd components/wasm && wasm-pack build
	tar -zcpf $(WASM_PKG) components/wasm/pkg

single:
	@./scripts/devnet/stopnodes.sh
	@./scripts/devnet/resetsingle.sh
	@./scripts/devnet/startsingle.sh

devnet:
	@./scripts/devnet/stopnodes.sh
	@./scripts/devnet/resetnodes.sh 3 1
	@./scripts/devnet/startnodes.sh

ci_build_image:
	@if [ ! -d "release/bin/" ] && [ -d "debug/bin" ]; then \
		mkdir -p release/bin/; \
		cp debug/bin/abci_validator_node debug/bin/query_server debug/bin/tendermint release/bin/; \
	fi
	docker build -t $(ECR_URL)/$(ENV)/abci_validator_node:$(IMAGE_TAG) -f container/Dockerfile-CI-abci_validator_node .
	docker build -t $(ECR_URL)/$(ENV)/query_server:$(IMAGE_TAG) -f container/Dockerfile-CI-query_server .
	docker build -t $(ECR_URL)/$(ENV)/tendermint:$(IMAGE_TAG) -f container/Dockerfile-CI-tendermint .
ifeq ($(ENV),release)
	docker tag $(ECR_URL)/$(ENV)/abci_validator_node:$(IMAGE_TAG) $(ECR_URL)/$(ENV)/abci_validator_node:latest
	docker tag $(ECR_URL)/$(ENV)/query_server:$(IMAGE_TAG) $(ECR_URL)/$(ENV)/query_server:latest
	docker tag $(ECR_URL)/$(ENV)/tendermint:$(IMAGE_TAG) $(ECR_URL)/$(ENV)/tendermint:latest
endif

ci_push_image:
	docker push $(ECR_URL)/$(ENV)/abci_validator_node:$(IMAGE_TAG)
	docker push $(ECR_URL)/$(ENV)/query_server:$(IMAGE_TAG)
	docker push $(ECR_URL)/$(ENV)/tendermint:$(IMAGE_TAG)
ifeq ($(ENV),release)
	docker push $(ECR_URL)/$(ENV)/abci_validator_node:latest
	docker push $(ECR_URL)/$(ENV)/query_server:latest
	docker push $(ECR_URL)/$(ENV)/tendermint:latest
endif

clean_image:
	docker rmi $(ECR_URL)/$(ENV)/abci_validator_node:$(IMAGE_TAG)
	docker rmi $(ECR_URL)/$(ENV)/query_server:$(IMAGE_TAG)
	docker rmi $(ECR_URL)/$(ENV)/tendermint:$(IMAGE_TAG)
ifeq ($(ENV),release)
	docker rmi $(ECR_URL)/$(ENV)/abci_validator_node:latest
	docker rmi $(ECR_URL)/$(ENV)/query_server:latest
	docker rmi $(ECR_URL)/$(ENV)/tendermint:latest
endif

####@./scripts/devnet/snapshot.sh <user_nick> <password> <token_name> <max_units> <genesis_issuance> <memo> <memo_updatable>
snapshot:
	@./scripts/devnet/snapshot.sh Findora my_pass FRA 21210000000000000 21000000000000000 my_memo N

####@./scripts/devnet/resetnodes.sh <num_of_validator_nodes> <num_of_normal_nodes>
mainnet:
	@./scripts/devnet/stopnodes.sh
	@./scripts/devnet/resetnodes.sh 4 1
