//!
//! # Integration testing
//!
//! - The number of validators should be 20
//! - The corresponding voting power of each validator should be correct
//! - The state of each validator should be `online`(the online/offline mark in `fn show`)
//! - All major functions should run well, such as transfer FRAs, custom assets, delegation ...
//! - Should be able to create 10 new blocks without any errors after the above operations
//!

use {
    crate::{sleep_n_block, VALIDATOR_LIST},
    finutils::common::{
        create_asset_x, delegate_x, get_serv_addr, issue_asset_x,
        transfer_asset_batch_x, undelegate_x,
        utils::{get_asset_balance, get_balance, get_validator_detail},
    },
    ledger::{
        data_model::{gen_random_keypair, TX_FEE_MIN},
        staking::FRA,
    },
    ruc::*,
    serde::Deserialize,
};

// 1. check the initial state
//     - the number of validators should be 20
//     - the power of validators should be [400_0000, 401_0000, ..., 419_0000]
//     - the `is_online` state of every validator should be 'true'
//     - the number of delegators should be 0
// 2. use validator[0] to define and issue a new asset A
// 3. wait 1.2 blocks
//     - check the balance of asset A of validator[0]
// 4. transfer asset A from validator[0] to validator[1..19]
// 5. wait 1.2 blocks
//     - check the balance of asset A of validator[1..19]
// 6. validators delegate 1 FRA unit to each other
//     - validator[0] --> validator[1]
//     - validator[2] --> validator[3]
//     - ...
//     - validator[19] --> validator[0]
// 7. wait 1.2 blocks
//     - check the delegators of each validator
// 8. wait 3 blocks
//     - check the power of each validator via ':26657/validators'
// 9. store the current power of validator[0], and unstake it
// 10. wait 5 blocks
//     - validator[0] should disappear from the results of ':26657/validators'
//     - the number of delegators of validator[0] should be 1
//     - its power should be 0
// 11. validator[0] delegate 1 FRA unit to itself
// 12. wait 5 blocks
//     - validator[0] should appear in the results of ':26657/validators' again
//     - its power should approximately equal to the value at < step 11 >
//     - the number of delegators of validator[0] should be 1
// 13. validator[18] delegate 1 FRA unit to validator[0]
// 14. wait 3 blocks
//     - the number of delegators of validator[0] should be 2
// 15. transfer all balances of validator[1..19] to validator[0]
// 16. wait 1.2 blocks
//     - all balances of validator[1..19] should be 0
// 17. trnasfer 100001 to 10 random addresses
// 18. wait 1.2 blocks
//     - check balances of the random addresses created in < step 17 >
// 19. delegate 10 times to validator[1] using the random addresses created in < step 17 >
// 10. wait 5 blocks
//     - check the number of delegators of validator[1]
//     - check the power of validator[1]
// 21. store the current block height, and wait 1.2 blocks
//     - the new block height should be bigger the the old one
//     - the number of validators should be 20
//     - the `is_online` state of every validator should be 'true'
//     - the power of validators should approximately be [400_0000, ..., 419_0000]
//     - the number of delegators of each validator should be 1, except validator[0]
// 22. repeat < step 15 > 4 times
pub fn run_all() -> Result<()> {
    let sa = get_serv_addr().c(d!())?;
    let v_set = VALIDATOR_LIST.values().collect::<Vec<_>>();

    for (i, v) in v_set.iter().skip(1).rev().enumerate() {
        assert!((i as u64 * 1_0000 + 2) * FRA > get_balance(&v.keypair).c(d!())?);
    }

    // 1.
    println!(">>> Check new validators ...");

    let vs = get_26657_validators(sa).c(d!())?;
    assert_eq!(v_set.len(), vs.validators.len());
    for v in vs.validators.iter() {
        let vd = get_validator_detail(&v.address).c(d!())?;
        assert!(vd.is_online);
        assert_eq!(0, vd.delegator_cnt);
        assert!(
            vs.block_height_int < 5 + vd.cur_height
                && vs.block_height_int >= vd.cur_height
        );
        assert!((0..v_set.len() as u64)
            .any(|i| (400_0000 + 1_0000 * i) * FRA == v.voting_power_int));
    }

    // 2.
    let v0_address = vs.validators[0].address.clone();
    let v0_kp = &v_set
        .iter()
        .find(|v| v.td_addr == vs.validators[0].address)
        .c(d!())?
        .keypair;

    println!(">>> Create custom asset A ...");
    let code = create_asset_x(v0_kp, "A", 9, None, true, None).c(d!())?;
    println!(">>> Wait 1.2 block ...");
    sleep_n_block!(1.2);

    println!(">>> Issue custom asset A ...");
    issue_asset_x(v0_kp, &code, 123456, false).c(d!())?;

    // 3.
    println!(">>> Wait 1.2 block ...");
    sleep_n_block!(1.2);

    assert_eq!(123456, get_asset_balance(v0_kp, Some(code)).c(d!())?);

    // 4.
    println!(">>> Transfer custom asset A ...");
    println!(">>> Wait 4.8 block for 4 times of transfering ...");

    let target_kps = v_set
        .iter()
        .map(|v| &v.keypair)
        .filter(|kp| kp.get_pk() != v0_kp.get_pk())
        .collect::<Vec<_>>();
    let targets = target_kps.iter().map(|kp| kp.get_pk()).collect::<Vec<_>>();
    transfer_asset_batch_x(v0_kp, &targets, Some(code), 1, true, true).c(d!())?;
    sleep_n_block!(1.2);
    transfer_asset_batch_x(v0_kp, &targets, Some(code), 1, false, true).c(d!())?;
    sleep_n_block!(1.2);
    transfer_asset_batch_x(v0_kp, &targets, Some(code), 1, true, false).c(d!())?;
    sleep_n_block!(1.2);
    transfer_asset_batch_x(v0_kp, &targets, Some(code), 1, false, false).c(d!())?;
    sleep_n_block!(1.2);

    // 5.
    println!(">>> Check balances ...");

    assert_eq!(
        123456 - 4 * targets.len() as u64,
        get_asset_balance(v0_kp, Some(code)).c(d!())?
    );

    for t in target_kps.iter() {
        assert_eq!(4, get_asset_balance(t, Some(code)).c(d!())?);
    }

    // 6.
    println!(">>> Delegate each other amoung validators...");

    for (i, v) in v_set.iter().enumerate() {
        delegate_x(&v.keypair, 1, &v_set[(1 + i) % v_set.len()].td_addr).c(d!())?;
    }

    // 7.
    println!(">>> Wait 3 block ...");
    sleep_n_block!(3);

    println!(">>> Check delegators of each validators ...");

    for v in v_set.iter() {
        assert_eq!(1, get_validator_detail(&v.td_addr).c(d!())?.delegator_cnt);
    }

    // 8.
    println!(">>> Wait 3 block ...");
    sleep_n_block!(3);

    println!(">>> Check voting power of each validators ...");

    let vs = get_26657_validators(sa).c(d!())?;
    for v in vs.validators.iter() {
        assert_eq!(
            v.voting_power_int,
            get_validator_detail(&v.address).c(d!())?.voting_power
        );
        assert!((0..v_set.len() as u64)
            .any(|i| 1 + (400_0000 + 1_0000 * i) * FRA == v.voting_power_int));
    }

    // 9.
    println!(">>> Undelegate validator-0 ...");

    let v0_power = vs.validators[0].voting_power_int;
    undelegate_x(v0_kp, None).c(d!())?;

    // 10.
    println!(">>> Wait 5 block ...");
    sleep_n_block!(5);

    println!(">>> Check changes of validator-0 ...");

    let vs = get_26657_validators(sa).c(d!())?;
    assert_eq!(v_set.len() - 1, vs.validators.len());
    assert!(!vs.validators.iter().any(|v| v.address == v0_address));

    let vd = get_validator_detail(&v0_address).c(d!())?;
    // NOTE: a tmp-delegator has been created while un-staking
    assert_eq!(2, vd.delegator_cnt);
    assert_eq!(0, vd.voting_power);

    // 11.
    println!(">>> Validator-0 do self-delegation again ...");

    delegate_x(v0_kp, 1, &v0_address).c(d!())?;

    // 12.
    println!(">>> Wait 5 block ...");
    sleep_n_block!(5);

    println!(">>> Check changes of validator-0...");

    let vs = get_26657_validators(sa).c(d!())?;
    assert_eq!(v_set.len(), vs.validators.len());
    assert!(vs.validators.iter().any(|v| v.address == v0_address));

    let vd = get_validator_detail(&v0_address).c(d!())?;
    // NOTE: a tmp-delegator has been created while un-staking
    assert_eq!(2, vd.delegator_cnt);
    assert_eq!(1 + v0_power, vd.voting_power);

    // 13.
    println!(">>> Validator-18 delegate to validator-0...");

    let v18_kp = &v_set
        .iter()
        .find(|v| v.td_addr == vs.validators[18].address)
        .c(d!())?
        .keypair;
    delegate_x(v18_kp, 1, &v0_address).c(d!())?;

    // 14.
    println!(">>> Wait 3 block ...");
    sleep_n_block!(3);

    println!(">>> Check changes of validator-0...");

    let vd = get_validator_detail(&v0_address).c(d!())?;
    // NOTE: a tmp-delegator has been created while un-staking
    assert_eq!(3, vd.delegator_cnt);
    assert_eq!(2 + v0_power, vd.voting_power);

    // 15.
    println!(">>> Transfer all balances to validator-0...");

    let mut balances = get_balance(&v_set[0].keypair).c(d!())?;
    for v in v_set.iter().skip(1) {
        get_balance(&v.keypair).c(d!()).and_then(|mut n| {
            if TX_FEE_MIN < n {
                n -= TX_FEE_MIN;
                balances += n;
                transfer_asset_batch_x(
                    &v.keypair,
                    &[v_set[0].pubkey],
                    None,
                    n,
                    false,
                    false,
                )
                .c(d!())
            } else {
                Ok(())
            }
        })?;
    }

    // 16.
    println!(">>> Wait 1.2 block ...");
    sleep_n_block!(1.2);

    println!(">>> Check balances ...");
    assert_eq!(balances, get_balance(&v_set[0].keypair).c(d!())?);
    for v in v_set.iter().skip(1) {
        assert_eq!(0, get_balance(&v.keypair).c(d!())?);
    }

    // 17.
    println!(">>> Transfer to 10 random addresses ...");
    let rkps = (0..10).map(|_| gen_random_keypair()).collect::<Vec<_>>();
    let targets = rkps.iter().map(|kp| kp.get_pk()).collect::<Vec<_>>();
    transfer_asset_batch_x(
        &v_set[0].keypair,
        &targets,
        None,
        1 + TX_FEE_MIN,
        false,
        false,
    )
    .c(d!())?;

    // 18.
    println!(">>> Wait 1.2 block ...");
    sleep_n_block!(1.2);
    println!(">>> Check balances of the 10 random addresses ...");
    for k in rkps.iter() {
        assert_eq!(1 + TX_FEE_MIN, get_asset_balance(k, None).c(d!())?);
    }

    // 19.
    println!(">>> Delegate 10 times to validator[1] ...");
    let vd_power = get_validator_detail(&v_set[1].td_addr)
        .c(d!())?
        .voting_power;
    for k in rkps.iter() {
        delegate_x(k, 1, &v_set[1].td_addr).c(d!())?;
    }

    // 20.
    println!(">>> Wait 5 block ...");
    sleep_n_block!(5);
    println!(">>> Check the results of those delegations ...");
    let vd = get_validator_detail(&v_set[1].td_addr).c(d!())?;
    assert_eq!(11, vd.delegator_cnt);
    assert_eq!(10 + vd_power, vd.voting_power);
    let td_power = get_26657_validators(sa)
        .c(d!())?
        .validators
        .iter()
        .find(|v| v.address == v_set[1].td_addr)
        .c(d!())?
        .voting_power_int;
    assert_eq!(vd.voting_power, td_power);

    // 21./22.
    println!(">>> Check the next 6 blocks ...");

    for _ in 0..5 {
        let height = get_26657_validators(sa).c(d!())?.block_height_int;
        println!(">>> Wait 1.2 block ...");
        sleep_n_block!(1.2);

        let vs = get_26657_validators(sa).c(d!())?;
        assert!(height < vs.block_height_int);
        assert_eq!(v_set.len(), vs.validators.len());

        for v in vs.validators.iter() {
            let vd = get_validator_detail(&v.address).c(d!())?;
            assert!(vd.is_online);
            assert!(1 <= vd.delegator_cnt);
            assert!(
                vs.block_height_int < 5 + vd.cur_height
                    && vs.block_height_int >= vd.cur_height
            );
            assert!((0..v_set.len() as u64).any(|i| {
                let l_b = (400_0000 + 1_0000 * i) * FRA;
                let u_b = (10 + 400_0000 + 1_0000 * i) * FRA;
                l_b < v.voting_power_int && u_b > v.voting_power_int
            }));
        }
    }

    println!("\x1b[31;1mcongratulate! We passed all the tests!\x1b[0m");

    Ok(())
}

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Deserialize)]
struct TmValidators {
    result: TmValidatorsBody,
}

#[derive(Debug, Deserialize)]
struct TmValidatorsBody {
    block_height: String,
    #[serde(skip)]
    block_height_int: u64,
    validators: Vec<TmValidator>,
}

impl TmValidatorsBody {
    fn parse(&mut self) {
        self.block_height_int = to_int(&self.block_height);
        self.validators.iter_mut().for_each(|v| {
            v.voting_power_int = to_int(&v.voting_power);
        });
    }
}

#[derive(Debug, Deserialize)]
struct TmValidator {
    address: String,
    voting_power: String,
    #[serde(skip)]
    voting_power_int: u64,
}

fn get_26657_validators(sa: &str) -> Result<TmValidatorsBody> {
    let url = format!("{}:26657/validators", sa);
    attohttpc::get(url)
        .send()
        .c(d!())?
        .error_for_status()
        .c(d!())?
        .bytes()
        .c(d!())
        .and_then(|b| {
            serde_json::from_slice::<TmValidators>(&b)
                .map(|mut r| {
                    r.result.parse();
                    r.result
                })
                .c(d!())
        })
}

fn to_int(n: &str) -> u64 {
    n.parse::<u64>().unwrap()
}
