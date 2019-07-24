release_dir     = release
bin_dir         = bin
lib_dir         = lib
release_subdirs = $(bin_dir) $(lib_dir)
rust_dirs       = $(shell find . -name "Cargo.toml" | sed -e "s:/Cargo.toml::")
pick            = target/release

bin_files =                             \
        ./$(pick)/txn_builder_cli       \
        ./$(pick)/abci_validator_node   \
        ./$(pick)/check_merkle          \

lib_files =                                                         \
        core/$(pick)/libcore.rlib                                   \
        components/ledger_app/$(pick)/libledger_app.rlib            \
        components/api_service/$(pick)/libapi_service.rlib          \
        components/query_processor/$(pick)/libquery_processor.rlib  \
        components/txn_builder/$(pick)/libtxn_builder.rlib          \

release:  rust
	rm -rf $(release_dir)
	mkdir $(release_dir)
	cd $(release_dir); for i in $(release_subdirs); do mkdir $$i; done
	cp $(bin_files) $(release_dir)/$(bin_dir)
	cp $(lib_files) $(release_dir)/$(lib_dir)

rust:
	for i in $(rust_dirs); do (cd $$i; cargo build --release) || exit; done
