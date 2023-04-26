# Development log

## Change plan

> Features to be added.

#### v0.4

- Triple masking, and so on

## Change log

#### v0.3.38-release
- Features:
  - Prism++: Support any type of FRC20 and nft to UTXO
- Bugfix:
  - Fix transaction serialization error
  - Fix evm nonce increase when tx error.

#### v0.3.30-release

- EVM upgrading and optimization in TPS.
- Optimization and compatibility fixes in historical transactions replay.

#### v0.3.20-release

- Reduce log to single line.

#### v0.3.19-release

- Enable EVM substate at height `180_2500`.
- Feature of staker replacement

#### v0.3.18-release

- Add replace staker.

#### v0.3.12-release

- EVM error code handling
- Pending nonce fix

#### v0.3.8-release

- FRC20 update height to `149_7500`.

> Functions that have been added.

#### v0.3.7-release

- Update height to `148_3286`.

#### v0.3.6-release

- Temporary disable EVM function.

#### v0.3.4-release

- fix IERC20 compability.

#### v0.3.3-release

- update height to 142_9000.
- Fix lost utxo info in utxo.

#### v0.3.2-release

- Force internal transfer's receiver must be ETH address.

#### v0.3.1-release

- adjust target height to `142_5000`.

#### v0.3.0-release

- Staking (Yanked at the block height of 142_5000 !)
  - Fix second compute for per year.
  - Fix APY rate range.
- EVM
  - Ethereum Virtual Machine integration.
  - Internal transfers (UTXO <--> Account).
  - Web3 API support.

#### v0.2.12-release

- Fix overflow error for rewards.

#### v0.2.11-release (Yanked at the block height of 124_7000 !)

- Fix incorrect calculations about `Nonconfidential Balances`
- Optimize the calculation of `expected return rate`
- Optimize some history-style APIs

#### v0.2.9-release (Yanked at the block height of 121_0000 !)

- Add an extra `reserved account` to optimize the APY curve
- Fix a BUG about the calculation of commissions
  - validator will loss its proposer&commission rewards when a zero-amount delegation is found

#### v0.2.8-release (Yanked at the block height of 120_0000 !)

- Avoid to store historical transactions on chain
  - Resist some kinds of the replay attack

#### v0.2.7-release (Yanked !)

- Optimize the on-chain log of `DeliverTx`

#### v0.2.6-release (Yanked !)

- Optimize the performance of some APIs

#### v0.2.5-release (Yanked !)

- Fix incorrect APY-calculation

#### v0.2.4-release (Yanked !)

- Support generating offline transactions

#### v0.2.3-release (Yanked !)

- Adjust minimal staking amount
- Avoid punish unstaked validator and its delegators
- Upgrade rust to the 2021 edition

#### v0.2.2-release (Yanked !)

- Fix a BUG in the logic of some special partial undelegations
  - [Issue 75, #75](https://github.com/FindoraNetwork/platform/issues/75)
- Fix a BUG about the voting power in the logic of un-delegation
- Fix some issues in the history-style API about POS
- optimize the usage of 'bnc'
- Optimize ABCI checker
  - Avoid invalid transactions from being stored
- Add balance checker for coinbase
  - Avoid wrong rewards when the reward pool is empty
- Enhance stability by using seed nodes in `findorad init`

#### v0.2.1-release (Yanked !)

- Fix a BUG in delegation logic
  - [Issue 65, #65](https://github.com/FindoraNetwork/platform/issues/65)

#### v0.2.0-release (Yanked !)

- POS function added
- Code optimization
- Stability enhancement

#### v0.1.0-release (Yanked !)

**launched at April 2021**

- Transfer function with privacy attributes
