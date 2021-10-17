//!
//! # Integration testing
//!
//! - The number of validators should be 20
//! - The corresponding voting power of each validator should be correct
//! - The state of each validator should be `online`(the online/offline mark in `fn show`)
//! - All major functions should run well, such as transfer FRAs, custom assets, delegation ...
//! - Should be able to create 10 new blocks without any errors after the above operations
//!

use ruc::*;

// 1. wait 4 blocks
//     - the number of validators should be 20
//     - the power of validators should be [800_0000, 801_0000, ..., 819_0000]
//     - the `is_online` state of every validator should be 'true'
//     - the number of delegators should be 0
// 2. use validator[0] to define and issue a new asset A
// 3. wait 2 blocks
// 4. transfer some asset A from validator[0] to validator[1..19]
// 5. wait 2 blocks
// 6. check the balance of asset A of validator[1..19]
// 7. validators delegate 1 FRA unit to each other
//     - validator[0] --> validator[1]
//     - validator[2] --> validator[2]
//     - ...
//     - validator[19] --> validator[0]
// 8. wait 2 blocks
// 9. check the delegators of each validator
// 10. wait 3 blocks
//     - check the power of each validator via ':26657/validators'
// 11. store the current power of validator[0], and unstake it
// 12. wait 5 blocks
//     - validator[0] should disappear from the results of ':26657/validators'
//     - the number of delegators of validator[0] should be 0
// 13. validator[0] delegate 1 FRA unit to itself
// 14. wait 5 blocks
//     - validator[0] should appear in the results of ':26657/validators' again
//     - its power should approximately equal to the value at < step 11 >
// 15. store the current block height, and wait 2 blocks
//     - the new block height should be bigger the the old one
// 16. repeat < step 15 > 4 times
pub(super) fn run_all() -> Result<()> {
    todo!()
}
