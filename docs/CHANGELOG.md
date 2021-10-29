# Development log

## Change plan

> Features to be added.

#### v0.3.x

**plan to launch at November 2021**

- Smart contact, and so on

## Change log

> Functions that have been added.

#### v0.2.9-release

- Add an extra `reserved account` to promote the APY
- Fix a BUG about the calculation of commissions
    - validator will loss its proposer&commission rewards when a zero-amount delegation is found

#### v0.2.8-release

- Avoid to store historical transactions on chain
    - Resist some kinds of the replay attack

#### v0.2.7-release

- Optimize the on-chain log of `DeliverTx`

#### v0.2.6-release

- Optimize the performance of some APIs

#### v0.2.5-release

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
