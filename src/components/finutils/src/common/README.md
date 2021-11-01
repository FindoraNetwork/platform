### How to use 

* setup for testing
```shell
fn setup -S http://localhost
fn setup -O PATH_TO_MNEMNOIC
fn show -b
```

* transfer-erc20
```shell
fn transfer-erc20 --addr 0x45998E5b3F8645EDAF4853d1391b8DCD87696c8C --amount 1 -i 0x40c10f1900000000000000000000000045998e5b3f8645edaf4853d1391b8dcd87696c8c0000000000000000000000000000000000000000000000000000000000000001
```

-i : mint(amount) bytecode

---

* erc20-to-utxo
```shell
fn erc20-to-utxo --addr 0x45998E5b3F8645EDAF4853d1391b8DCD87696c8C --amount 1 -i 0x70a0823100000000000000000000000045998e5b3f8645edaf4853d1391b8dcd87696c8c --eth-key xxx
```
-i: burn(amount) bytecode
