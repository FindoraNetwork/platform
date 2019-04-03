# EIAN Platform
Top level targets: executable and plug-in components of the EIAN platform
* Tendermint ABCI executable that adapts the layer for performing validation and maintaining a ledger
* plug-in for Stellar-based scp_server that adapts the same layer
* any future executable and distributable components

```
+
+-- core/ - shared libraries providing common application support components
|    +-- data_model/ - common types
|    +== store/ = common resource lookup & retrieval
|    +== <module1>/
|    | ...
|    +== <moduleK>/
|
+-- components/ - capabilities that can be incorporated into one or more top level targets
|    +-- ledger_app/ - provides a ledger interface with transaction validation and post-transaction state updates.
|    +== <component1>/
|    | ...
|    +-- <componentM>
|
+-- <target1>/
+-- <target2>/
| ...
+-- <targetN>/

```
