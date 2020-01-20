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
bin_dir         = bin
lib_dir         = lib
pick            = target/release
release_dir     = release
release_subdirs = $(bin_dir) $(lib_dir)
rust_dirs       = $(shell find . -name target -prune -o    \
                                 -name git    -prune -o    \
                                 -name "Cargo.toml" -print \
                              | sed -e "s:/Cargo.toml::")

bin_files =                             \
        ./$(pick)/txn_builder_cli       \
        ./$(pick)/abci_validator_node   \
        ./$(pick)/check_merkle          \

lib_files =                                    \
        ./$(pick)/libledger.rlib               \
        ./$(pick)/libledger_app.rlib           \
        ./$(pick)/libapi_service.rlib          \
        ./$(pick)/libtxn_builder.rlib          \

release:  rust
	rm -rf $(release_dir)
	mkdir $(release_dir)
	cd $(release_dir); for i in $(release_subdirs); do mkdir $$i; done
	cp $(bin_files) $(release_dir)/$(bin_dir)
	cp $(lib_files) $(release_dir)/$(lib_dir)

rust:
	for i in $(rust_dirs); do (cd $$i; cargo build --release) || exit; done

test_status:
	scripts/incur build
	scripts/incur build --release
	scripts/incur test
	make release
