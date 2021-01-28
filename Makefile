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

bin_files =                             \
        ./$(pick)/abci_validator_node   \
        ./$(pick)/check_merkle          \
		./$(pick)/solvency_cli       	\
	   	./$(pick)/txn_cli       		\

lib_files =                                    \
        ./$(pick)/libledger.rlib               \
        ./$(pick)/libledger_api_service.rlib   \
		./$(pick)/libsolvency.rlib       	   \
        ./$(pick)/libtxn_builder.rlib          \
		./$(pick)/libtxn_cli.rlib              \
		./$(pick)/libwhitelist.rlib            \

define pack
	-@ rm -rf $(target_dir)
	mkdir $(target_dir)
	cd $(target_dir); for i in $(release_subdirs); do mkdir $$i; done
	cp $(bin_files) $(target_dir)/$(bin_dir)
	cp $(lib_files) $(target_dir)/$(lib_dir)
endef

build:
ifdef DBG
	cargo build
	$(call pack,$(target_dir))
else
	@ echo -e "\x1b[31;01m\$$(DBG) must be defined !\x1b[00m"
	@ exit 1
endif

build_release:
ifdef DBG
	@ echo -e "\x1b[31;01m\$$(DBG) must NOT be defined !\x1b[00m"
	@ exit 1
else
	cargo build --release
	$(call pack,$(target_dir))
endif

test:
	cargo test --lib --workspace -- --test-threads=1 --nocapture
	cargo test --workspace -- --test-threads=1 --nocapture

bench:
	cargo bench --workspace

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

single:
	@./scripts/devnet/stopnodes.sh
	@./scripts/devnet/resetsingle.sh
	@./scripts/devnet/startsingle.sh

devnet:
	@./scripts/devnet/stopnodes.sh
	@./scripts/devnet/resetnodes.sh
	@./scripts/devnet/startnodes.sh
