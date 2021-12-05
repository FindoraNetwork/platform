![](https://tokei.rs/b1/github/FindoraNetwork/platform)
![GitHub top language](https://img.shields.io/github/languages/top/FindoraNetwork/platform)
![GitHub Workflow Status](https://img.shields.io/github/workflow/status/FindoraNetwork/platform/Develop)
![Docker Pulls](https://img.shields.io/docker/pulls/findoranetwork/findorad)
![GitHub issues](https://img.shields.io/github/issues-raw/FindoraNetwork/platform)
![GitHub pull requests](https://img.shields.io/github/issues-pr-raw/FindoraNetwork/platform)


# Findora Platform

- [**Wiki**](https://wiki.findora.org/)
- [**Contribution Guide**](docs/contribution_guide.md)
- [**Change Log**](docs/CHANGELOG.md)

```shell
.
├── Cargo.toml
├── container/
├── docs/
├── LICENSE
├── Makefile
├── README.md
├── rustfmt.toml
├── rust-toolchain
├── src/
│   ├── components/
│   │   ├── abciapp/
│   │   ├── finutils/
│   │   └── wasm/
│   ├── ledger/
│   │   ├── build.rs
│   │   ├── Cargo.toml
│   │   └── src/
│   └── libs/
│       ├── bitmap/
│       ├── credentials/
│       ├── cryptohash/
│       ├── globutils/
│       ├── merkle_tree/
│       ├── README.md
│       └── sliding_set/
└── tools/
```

```
===============================================================================
 Language            Files        Lines         Code     Comments       Blanks
===============================================================================
 BASH                    1          106          101            1            4
 JavaScript              2          113           77           14           22
 JSON                    5          424          424            0            0
 Makefile                1          264          180           27           57
 Python                  1          138           99           19           20
 Shell                  15          777          571           83          123
 TOML                   12          418          349            7           62
 YAML                    1          399          379           18            2
-------------------------------------------------------------------------------
 Markdown               11          488            0          343          145
 |- Rust                 1           37           25            5            7
 |- Shell                4          114           80           23           11
 (Total)                            639          105          371          163
-------------------------------------------------------------------------------
 Rust                   69        26842        22060         1371         3411
 |- Markdown            59         1669           13         1543          113
 (Total)                          28511        22073         2914         3524
===============================================================================
 Total                   0        29969        24240         1883         3846
===============================================================================
```

### Licensing

The primary license for Platform is the Business Source License 1.1 (`BUSL-1.1`), see [`LICENSE`](./LICENSE).

### Exceptions

- All files in `components/contracts` are licensed under `Apache-2.0`

### SEE ALSO

- [**Zei**](https://github.com/FindoraNetwork/zei)
- [**fBNC**](https://github.com/FindoraNetwork/fbnc)
- [**BTM**](https://github.com/FindoraNetwork/btm)
- [**RUC**](https://github.com/FindoraNetwork/ruc)
