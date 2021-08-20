# Findora Platform

- [**compile & build**](docs/compile_build.md)
- [**contribution guide**](docs/contribution_guide.md)

Code structure:

```
src
├── components/
│   ├── abciapp/
│   │   └── src/
│   │       ├── abci/
│   │       ├── api/
│   │       │   ├── mod.rs
│   │       │   ├── query_server/    >>> old path: "components/{query_server,query_api,ledger_api}" <<<
│   │       │   └── submission_server/    >>> old path: "components/{submission_server,submission_api}" <<<
│   │       ├── bins/
│   │       │   └── findorad.rs    >>> old path: "components/abci_validator_node/src/abci_validator_node.rs" <<<
│   │       └── lib.rs
│   ├── finutils/
│   │   └── src/
│   │       ├── api/
│   │       ├── bins/
│   │       ├── common/
│   │       ├── lib.rs
│   │       └── txn_builder/    >>> old path: "components/txn_builder" <<<
│   └── wasm/
├── ledger/
│   └── src/
│       ├── data_model/
│       ├── lib.rs
│       ├── staking/
│       └── store.rs    >>> old path: "ledger/src/store/store.rs" <<<
└── libs/
    ├── bitmap/
    ├── credentials/
    ├── cryptohash/
    ├── globutils/
    │   └── src/
    │       ├── lib.rs    >>> old path: "libs/utils" <<<
    │       └── wallet.rs    >>> old path: "components/wallet" <<<
    ├── merkle_tree/    >>> old path: "libs/ads/append_only_merkle" <<<
    ├── README.md
    └── sliding_set/
```
