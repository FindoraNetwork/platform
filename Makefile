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

all: build_release_goleveldb

export CARGO_NET_GIT_FETCH_WITH_CLI = true
export PROTOC = $(shell which protoc)

export STAKING_INITIAL_VALIDATOR_CONFIG = $(shell pwd)/ledger/src/staking/init/staking_config.json
export STAKING_INITIAL_VALIDATOR_CONFIG_DEBUG_ENV = $(shell pwd)/ledger/src/staking/init/staking_config_debug_env.json
export STAKING_INITIAL_VALIDATOR_CONFIG_ABCI_MOCK = $(shell pwd)/ledger/src/staking/init/staking_config_abci_mock.json

export LEDGER_DIR=/tmp/findora
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
		./$(pick)/fns \
		./$(pick)/stt \
		./$(pick)/staking_cfg_generator

bin_files_musl_debug = \
		./${CARGO_TARGET_DIR}/x86_64-unknown-linux-musl/$(target_dir)/findorad \
		./${CARGO_TARGET_DIR}/x86_64-unknown-linux-musl/$(target_dir)/fns \
		./${CARGO_TARGET_DIR}/x86_64-unknown-linux-musl/$(target_dir)/stt \
		./${CARGO_TARGET_DIR}/x86_64-unknown-linux-musl/$(target_dir)/staking_cfg_generator

WASM_PKG = wasm.tar.gz
lib_files = ./$(WASM_PKG)

define pack
	-@ rm -rf $(target_dir)
	mkdir $(target_dir)
	cd $(target_dir); for i in $(release_subdirs); do mkdir $$i; done
	cp $(bin_files) $(target_dir)/$(bin_dir)
	cp $(target_dir)/$(bin_dir)/* ~/.cargo/bin/
endef

define pack_musl_debug
	-@ rm -rf $(target_dir)
	mkdir $(target_dir)
	cd $(target_dir); for i in $(release_subdirs); do mkdir $$i; done
	cp $(bin_files_musl_debug) $(target_dir)/$(bin_dir)
	cp $(target_dir)/$(bin_dir)/* ~/.cargo/bin/
endef

# Build for cleveldb
build:
ifdef DBG
	cargo build --bins -p abciapp -p fintools --no-default-features --features "cleveldb"
	$(call pack,$(target_dir))
else
	@ echo -e "\x1b[31;01m\$$(DBG) must be defined !\x1b[00m"
	@ exit 1
endif

# Build for cleveldb
build_release:
ifdef DBG
	@ echo -e "\x1b[31;01m\$$(DBG) must NOT be defined !\x1b[00m"
	@ exit 1
else
	cargo build --release --bins -p abciapp -p fintools --no-default-features --features "cleveldb"
	$(call pack,$(target_dir))
endif

# Build for goleveldb
build_goleveldb:
ifdef DBG
	cargo build --bins -p abciapp -p fintools
	$(call pack,$(target_dir))
else
	@ echo -e "\x1b[31;01m\$$(DBG) must be defined !\x1b[00m"
	@ exit 1
endif

# Build for goleveldb
build_release_goleveldb:
ifdef DBG
	@ echo -e "\x1b[31;01m\$$(DBG) must NOT be defined !\x1b[00m"
	@ exit 1
else
	cargo build --release --bins -p abciapp -p fintools
	$(call pack,$(target_dir))
endif

build_release_musl:
ifdef DBG
	@ echo -e "\x1b[31;01m\$$(DBG) must NOT be defined !\x1b[00m"
	@ exit 1
else
	cargo build --release --bins -p abciapp -p fintools --target=x86_64-unknown-linux-musl
	$(call pack_musl_debug,$(target_dir))
endif

build_release_debug:
ifdef DBG
	@ echo -e "\x1b[31;01m\$$(DBG) must NOT be defined !\x1b[00m"
	@ exit 1
else
	cargo build --features="debug_env" --release --bins -p abciapp -p fintools
	$(call pack,$(target_dir))
endif

build_release_musl_debug:
ifdef DBG
	@ echo -e "\x1b[31;01m\$$(DBG) must NOT be defined !\x1b[00m"
	@ exit 1
else
	cargo build --features="debug_env" --release --bins -p abciapp -p fintools --target=x86_64-unknown-linux-musl
	$(call pack_musl_debug,$(target_dir))
endif

test:
	cargo test --release --workspace -- --test-threads=1
	cargo test --release --features="abci_mock" node -- --test-threads=1
	cargo test --release --workspace -- --ignored

staking_test:
	$(unset LEDGER_DIR)
	cargo test --release staking -- --test-threads=1 --nocapture
	cargo test --release staking --features="abci_mock" -- --test-threads=1 --nocapture

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

test_status:
	scripts/incur build
	scripts/incur build --release
	scripts/incur test
	$(MAKE) build_release

fmt:
	@ cargo fmt

fmtall:
	@ bash ./tools/fmt.sh

clean:
	@ cargo clean
	@ rm -rf tools/tendermint .git/modules/tools/tendermint
	@ rm -rf debug release Cargo.lock

wasm:
	cd components/wasm && wasm-pack build
	tar -zcpf $(WASM_PKG) components/wasm/pkg

single:
	@./scripts/devnet/stopnodes.sh
	@./scripts/devnet/resetsingle.sh
	@./scripts/devnet/startsingle.sh

devnet:
	@./scripts/devnet/stopnodes.sh
	@./scripts/devnet/resetnodes.sh 20 1
	@./scripts/devnet/startnodes.sh

debug_env: stop_debug_env build_release_debug
	@- rm -rf $(LEDGER_DIR)
	@ mkdir $(LEDGER_DIR)
	@ cp tools/debug_env.tar.gz $(LEDGER_DIR)/
	@ cd $(LEDGER_DIR) && tar -xpf debug_env.tar.gz && mv debug_env devnet
	@ ./scripts/devnet/startnodes.sh

run_staking_demo:
	bash tools/staking/demo.sh

start_debug_env:
	./scripts/devnet/startnodes.sh

stop_debug_env:
	@./scripts/devnet/stopnodes.sh

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

	docker push $(PRIVATE_ECR_URL)/$(ENV)/findorad:$(IMAGE_TAG)
ifeq ($(ENV),release)
	docker push $(PUBLIC_ECR_URL)/$(ENV)/findorad:latest
endif

clean_image:

	docker rmi $(PRIVATE_ECR_URL)/$(ENV)/findorad:$(IMAGE_TAG)
ifeq ($(ENV),release)
	docker rmi $(PUBLIC_ECR_URL)/$(ENV)/findorad:latest
endif


####@./scripts/devnet/snapshot.sh <user_nick> <password> <token_name> <max_units> <genesis_issuance> <memo> <memo_updatable>
snapshot:
	@./scripts/devnet/snapshot.sh Findora my_pass FRA 21210000000000000 21000000000000000 my_memo N

network:
	@./scripts/devnet/startnetwork.sh Findora my_pass FRA 21210000000000000 21000000000000000 my_memo N

####@./scripts/devnet/resetnodes.sh <num_of_validator_nodes> <num_of_normal_nodes>
mainnet:
	@./scripts/devnet/stopnodes.sh
	@./scripts/devnet/resetnodes.sh 4 4
