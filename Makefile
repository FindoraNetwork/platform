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
		./$(pick)/abci_validator_node \
		./$(pick)/query_server \
		./$(pick)/check_merkle \
		./$(pick)/solvency_cli \
		./$(pick)/txn_cli \
		./$(pick)/findora \
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

build: tendermint wasm
ifdef DBG
	cargo build --workspace --exclude wasm --exclude http_tester --exclude log_tester
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
	cargo build --release --workspace --exclude wasm --exclude http_tester --exclude log_tester
	$(call pack,$(target_dir))
endif

test:
	cargo test --lib --workspace -- --test-threads=1
	cargo test --workspace -- --test-threads=1

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
	bash ./tools/fmt.sh

clean:
	@ cargo clean
	@ rm -rf debug release Cargo.lock

tendermint:
	if [ -d ".git" ]; then \
		git submodule update --init --recursive; \
	else \
		if [ -d "tools/tendermint" ]; then rm -rf tools/tendermint; fi; \
		git clone -b v0.33.5 --depth=1 https://gitee.com/kt10/tendermint.git tools/tendermint; \
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

####@./scripts/devnet/resetnodes.sh <num_of_validator_nodes> <num_of_normal_nodes>
####@./scripts/devnet/snapshot.sh <user_nick> <password> <token_name> <max_units> <genesis_issuance> <memo> <memo_updatable>
snapshot:
	@./scripts/devnet/stopnodes.sh
	@./scripts/devnet/resetnodes.sh 4 1
	@./scripts/devnet/snapshot.sh Findora my_pass FRA 21210000000000000 21000000000000000 my_memo Y
