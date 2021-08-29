#
#
#

all: build_release_goleveldb

export CARGO_NET_GIT_FETCH_WITH_CLI = true
export PROTOC = $(shell which protoc)

export STAKING_INITIAL_VALIDATOR_CONFIG = $(shell pwd)/src/ledger/src/staking/init/staking_config.json
export STAKING_INITIAL_VALIDATOR_CONFIG_DEBUG_ENV = $(shell pwd)/src/ledger/src/staking/init/staking_config_debug_env.json
export STAKING_INITIAL_VALIDATOR_CONFIG_ABCI_MOCK = $(shell pwd)/src/ledger/src/staking/init/staking_config_abci_mock.json

export DEBUG_DIR=/tmp/findora
export ENABLE_LEDGER_SERVICE = true
export ENABLE_QUERY_SERVICE = true

ifndef CARGO_TARGET_DIR
	export CARGO_TARGET_DIR=target
endif

$(info ====== Build root is "$(CARGO_TARGET_DIR)" ======)

ifdef DBG
target_dir = debug
else
target_dir = release
endif

bin_dir         = bin
lib_dir         = lib
pick            = ${CARGO_TARGET_DIR}/$(target_dir)
release_subdirs = $(bin_dir) $(lib_dir)

bin_files = \
		./$(pick)/findorad \
		./$(pick)/abcid \
		$(shell go env GOPATH)/bin/tendermint \
		./$(pick)/fns \
		./$(pick)/stt \
		./$(pick)/staking_cfg_generator

WASM_PKG = wasm.tar.gz
lib_files = ./$(WASM_PKG)

define pack
	-@ rm -rf $(target_dir)
	mkdir $(target_dir)
	cd $(target_dir); for i in $(release_subdirs); do mkdir $$i; done
	cp $(bin_files) $(target_dir)/$(bin_dir)
	cp $(target_dir)/$(bin_dir)/* ~/.cargo/bin/
	cd $(target_dir)/$(bin_dir)/ && findorad pack
	cp -f /tmp/findorad $(target_dir)/$(bin_dir)/
	cp -f /tmp/findorad ~/.cargo/bin/
endef

# Build for cleveldb
build: tendermint_cleveldb
ifdef DBG
	cargo build --bins -p abciapp -p finutils
	$(call pack,$(target_dir))
else
	@ echo -e "\x1b[31;01m\$$(DBG) must be defined !\x1b[00m"
	@ exit 1
endif

# Build for cleveldb
build_release: tendermint_cleveldb
ifdef DBG
	@ echo -e "\x1b[31;01m\$$(DBG) must NOT be defined !\x1b[00m"
	@ exit 1
else
	cargo build --release --bins -p abciapp -p finutils
	$(call pack,$(target_dir))
endif

# Build for goleveldb
build_goleveldb: tendermint_goleveldb
ifdef DBG
	cargo build --bins -p abciapp -p finutils
	$(call pack,$(target_dir))
else
	@ echo -e "\x1b[31;01m\$$(DBG) must be defined !\x1b[00m"
	@ exit 1
endif

# Build for goleveldb
build_release_goleveldb: tendermint_goleveldb
ifdef DBG
	@ echo -e "\x1b[31;01m\$$(DBG) must NOT be defined !\x1b[00m"
	@ exit 1
else
	cargo build --release --bins -p abciapp -p finutils
	$(call pack,$(target_dir))
endif

build_release_debug: tendermint_goleveldb
ifdef DBG
	@ echo -e "\x1b[31;01m\$$(DBG) must NOT be defined !\x1b[00m"
	@ exit 1
else
	cargo build --features="debug_env" --release --bins -p abciapp -p finutils
	$(call pack,$(target_dir))
endif

tendermint_cleveldb:
	bash -x tools/download_tendermint.sh 'tools/tendermint'
	mkdir -p $(shell go env GOPATH)/bin
	cd tools/tendermint \
		&& $(MAKE) build TENDERMINT_BUILD_OPTIONS=cleveldb \
		&& cp build/tendermint $(shell go env GOPATH)/bin/

tendermint_goleveldb:
	bash -x tools/download_tendermint.sh 'tools/tendermint'
	cd tools/tendermint && $(MAKE) install

test:
	# cargo test --release --workspace -- --test-threads=1 # --nocapture
	cargo test --release --workspace -- --ignored # --nocapture
	cargo test --release --features="abci_mock" -- --test-threads=1 # --nocapture

coverage:
	cargo tarpaulin --timeout=900 --branch --workspace --release --ignored --features="abci_mock" \
		|| cargo install cargo-tarpaulin \
		&& cargo tarpaulin --timeout=900 --branch --workspace --release --ignored --features="abci_mock"

staking_cfg:
	cargo run --bin staking_cfg_generator

bench:
	cargo bench --workspace

lint:
	cargo clippy --workspace
	cargo clippy --workspace --tests
	cargo clippy --features="abci_mock" --workspace --tests

update:
	cargo update

fmt:
	@ cargo fmt

fmtall:
	@ bash ./tools/fmt.sh

clean:
	@ cargo clean
	@ rm -rf tools/tendermint .git/modules/tools/tendermint
	@ rm -rf debug release Cargo.lock

cleanall: clean
	@ git clean -fdx

wasm:
	cd src/components/wasm && wasm-pack build
	tar -zcpf $(WASM_PKG) src/components/wasm/pkg

single:
	@./tools/devnet/stopnodes.sh
	@./tools/devnet/resetsingle.sh
	@./tools/devnet/startsingle.sh

devnet:
	@./tools/devnet/stopnodes.sh
	@./tools/devnet/resetnodes.sh 20 1
	@./tools/devnet/startnodes.sh

debug_env: stop_debug_env build_release_debug
	@- rm -rf $(DEBUG_DIR)
	@ mkdir $(DEBUG_DIR)
	@ cp tools/debug_env.tar.gz $(DEBUG_DIR)/
	@ cd $(DEBUG_DIR) && tar -xpf debug_env.tar.gz && mv debug_env devnet
	@ ./tools/devnet/startnodes.sh

run_staking_demo: stop_debug_env
	bash tools/staking/demo.sh

start_debug_env:
	bash ./tools/devnet/startnodes.sh

stop_debug_env:
	bash ./tools/devnet/stopnodes.sh

join_qa01: stop_debug_env
	bash tools/node_init.sh qa01

join_qa02: stop_debug_env
	bash tools/node_init.sh qa02

join_testnet: stop_debug_env
	bash tools/node_init.sh testnet

join_mainnet: stop_debug_env
	bash tools/node_init.sh mainnet

# ci_build_image:
# 	@if [ ! -d "release/bin/" ] && [ -d "debug/bin" ]; then \
# 		mkdir -p release/bin/; \
# 		cp debug/bin/findorad release/bin/; \
# 	fi
# 	docker build -t $(ECR_URL)/$(ENV)/abci_validator_node:$(IMAGE_TAG) -f container/Dockerfile-CI-abci_validator_node .
# ifeq ($(ENV),release)
# 	docker tag $(ECR_URL)/$(ENV)/abci_validator_node:$(IMAGE_TAG) $(ECR_URL)/$(ENV)/findorad:latest
# endif

# ci_push_image:
# 	docker push $(ECR_URL)/$(ENV)/abci_validator_node:$(IMAGE_TAG)
# ifeq ($(ENV),release)
# 	docker push $(ECR_URL)/$(ENV)/abci_validator_node:latest
# endif

# clean_image:
# 	docker rmi $(ECR_URL)/$(ENV)/abci_validator_node:$(IMAGE_TAG)
# ifeq ($(ENV),release)
# 	docker rmi $(ECR_URL)/$(ENV)/abci_validator_node:latest
# endif


ci_build_image:
	@if [ ! -d "release/bin/" ] && [ -d "debug/bin" ]; then \
		mkdir -p release/bin/; \
		cp debug/bin/findorad release/bin/; \
	fi
	docker build -t $(PUBLIC_ECR_URL)/$(ENV)/findorad:$(IMAGE_TAG) -f container/Dockerfile-CI-findorad .
ifeq ($(ENV),release)
	docker tag $(PUBLIC_ECR_URL)/$(ENV)/findorad:$(IMAGE_TAG) $(PUBLIC_ECR_URL)/$(ENV)/findorad:latest
endif

ci_push_image:
	docker push $(PUBLIC_ECR_URL)/$(ENV)/findorad:$(IMAGE_TAG)
ifeq ($(ENV),release)
	docker push $(PUBLIC_ECR_URL)/$(ENV)/findorad:latest
endif

clean_image:
	docker rmi $(PUBLIC_ECR_URL)/$(ENV)/findorad:$(IMAGE_TAG)
ifeq ($(ENV),release)
	docker rmi $(PUBLIC_ECR_URL)/$(ENV)/findorad:latest
endif

ci_build_image_dockerhub:
	@if [ ! -d "release/bin/" ] && [ -d "debug/bin" ]; then \
		mkdir -p release/bin/; \
		cp debug/bin/findorad release/bin/; \
	fi
	docker build -t $(DOCKERHUB_URL)/findorad:$(IMAGE_TAG) -f container/Dockerfile-CI-findorad .
ifeq ($(ENV),release)
	docker tag $(DOCKERHUB_URL)/findorad:$(IMAGE_TAG) $(DOCKERHUB_URL)/findorad:latest
endif

ci_push_image_dockerhub:
	docker push $(DOCKERHUB_URL)/findorad:$(IMAGE_TAG)
ifeq ($(ENV),release)
	docker push $(DOCKERHUB_URL)/findorad:latest
endif

clean_image_dockerhub:
	docker rmi $(DOCKERHUB_URL)/findorad:$(IMAGE_TAG)
ifeq ($(ENV),release)
	docker rmi $(DOCKERHUB_URL)/findorad:latest
endif


####@./tools/devnet/snapshot.sh <user_nick> <password> <token_name> <max_units> <genesis_issuance> <memo> <memo_updatable>
snapshot:
	@./tools/devnet/snapshot.sh Findora my_pass FRA 21210000000000000 21000000000000000 my_memo N

network:
	@./tools/devnet/startnetwork.sh Findora my_pass FRA 21210000000000000 21000000000000000 my_memo N

####@./tools/devnet/resetnodes.sh <num_of_validator_nodes> <num_of_normal_nodes>
mainnet:
	@./tools/devnet/stopnodes.sh
	@./tools/devnet/resetnodes.sh 4 4
