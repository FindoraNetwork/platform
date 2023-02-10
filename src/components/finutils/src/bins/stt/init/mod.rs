//!
//! # init
//!
//! Initialize a new network environment.
//!

pub mod i_testing;

use {
    super::*,
    finutils::common::{transfer_asset_batch_x, utils::get_balance},
    ledger::data_model::TX_FEE_MIN,
    ledger::staking::td_addr_to_string,
};

pub fn init(
    mut interval: u64,
    is_mainnet: bool,
    skip_validator: bool,
    staking_mnemonics: Option<&str>,
) -> Result<()> {
    if 0 == interval {
        interval = *BLOCK_INTERVAL;
    }

    let mut validators = vec![];
    let mut key_pairs = vec![];
    if !skip_validator {
        if let Some(path) = staking_mnemonics {
            println!(">>> Set initial validator set (Customed) ...");
            (validators, key_pairs) =
                common::set_initial_validators_with_mnemonics(path).c(d!())?;
        } else {
            println!(">>> Set initial validator set ...");
            common::set_initial_validators().c(d!())?;
        }
    }

    if is_mainnet {
        Ok(())
    } else {
        println!(">>> Wait 1 block ...");
        sleep_n_block!(1, interval);

        let root_kp =
            wallet::restore_keypair_from_mnemonic_default(ROOT_MNEMONIC).c(d!())?;

        println!(">>> Block interval: {interval} seconds",);
        println!(">>> Define and issue FRA ...");
        common::utils::send_tx(&fra_gen_initial_tx(&root_kp)).c(d!())?;

        println!(">>> Wait 1.6 block ...");
        sleep_n_block!(1.6, interval);

        if skip_validator {
            println!(">>> DONE !");
            return Ok(());
        }

        if staking_mnemonics.is_some() {
            let target_list = key_pairs.iter().zip(validators.iter())
                .map(|(kp,v)| (kp.get_pk_ref(), v.td_power + 10000000))
                .collect::<Vec<_>>();

            println!(">>> Transfer FRAs to validators ...");
            common::utils::transfer_batch(&root_kp, target_list, None, false, false)
                .c(d!())?;

            println!(">>> Wait 1.6 block ...");
            sleep_n_block!(1.6, interval);

            println!(">>> Propose self-delegations ...");
            for (_i, (owner_kp, validator)) in
                key_pairs.into_iter().zip(validators).enumerate()
            {
                let amount = validator.td_power;

                let mut builder = common::utils::new_tx_builder().c(d!())?;

                let principal_op = common::utils::gen_transfer_op(
                    &owner_kp,
                    vec![(&BLACK_HOLE_PUBKEY_STAKING, amount)],
                    None,
                    false,
                    false,
                    Some(
                        zei::xfr::asset_record::AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                    ),
                )
                .c(d!())?;

                builder
                    .add_operation(principal_op)
                    .add_operation_delegation(
                        &owner_kp,
                        amount,
                        td_addr_to_string(&validator.td_addr),
                    );
                    
                let mut tx = builder.take_transaction();

                tx.sign_to_map(&owner_kp);
                common::utils::send_tx(&tx).c(d!())?;
            }

            println!(">>> DONE !");
            return Ok(());
        }

        let mut target_list = USER_LIST
            .values()
            .map(|u| &u.pubkey)
            .chain(VALIDATOR_LIST.values().map(|v| &v.pubkey))
            .map(|pk| (pk, FRA_PRE_ISSUE_AMOUNT / 2_000))
            .collect::<Vec<_>>();

        // Wallet Address: fra18xkez3fum44jq0zhvwq380rfme7u624cccn3z56fjeex6uuhpq6qv9e4g5
        // Mnemonic: field ranch pencil chest effort coyote april move injury illegal forest amount bid sound mixture use second pet embrace twice total essay valve loan
        // Key: {
        //   "pub_key": "Oa2RRTzdayA8V2OBE7xp3n3NKrjGJxFTSZZybXOXCDQ=",
        //   "sec_key": "Ew9fMaryTL44ZXnEhcF7hQ-AB-fxgaC8vyCH-hCGtzg="
        // }
        let bank = pnk!(wallet::public_key_from_base64(
            "Oa2RRTzdayA8V2OBE7xp3n3NKrjGJxFTSZZybXOXCDQ="
        ));
        target_list.push((&bank, FRA_PRE_ISSUE_AMOUNT / 100 * 98));

        println!(">>> Transfer FRAs to validators ...");
        common::utils::transfer_batch(&root_kp, target_list, None, true, true)
            .c(d!())?;

        println!(">>> Wait 1.2 block ...");
        sleep_n_block!(1.2);

        println!(">>> Re-distribution ...");
        println!(">>> Wait 2.4 block ...");
        re_distribution().c(d!())?;

        println!(">>> Propose self-delegations ...");
        for (i, v) in VALIDATOR_LIST.values().enumerate() {
            delegate::gen_tx(&v.name, (400_0000 + i as u64 * 1_0000) * FRA, &v.name)
                .c(d!())
                .and_then(|tx| common::utils::send_tx(&tx).c(d!()))?;
        }

        println!(">>> Wait 5 block ...");
        sleep_n_block!(5);

        println!(">>> Init work done !");

        println!(">>> Start running integration tests ...");

        i_testing::run_all().c(d!())
    }
}

// 1. transfer all balances of validator[1..19] to validator[0]
// 2. wait 2 blocks
// 3. check whether the total amount is bigger than 8400_0000 FRAs
// 4. transfer back to validator[1..19] from validator[0]
// 5. wait 2 blocks
// 6. check the balance of each validator
fn re_distribution() -> Result<()> {
    let v_set = VALIDATOR_LIST.values().collect::<Vec<_>>();

    // 1.
    for v in v_set.iter().skip(1) {
        get_balance(&v.keypair).c(d!()).and_then(|n| {
            if TX_FEE_MIN < n {
                transfer_asset_batch_x(
                    &v.keypair,
                    &[v_set[0].pubkey],
                    None,
                    n - TX_FEE_MIN,
                    true,
                    true,
                )
                .c(d!())
            } else {
                Ok(())
            }
        })?;
    }

    // 2.
    sleep_n_block!(1.2);

    // 3.
    let total = get_balance(&v_set[0].keypair).c(d!())?;
    if 8400_0000 * FRA > total {
        return Err(eg!("Total balance is not enough !!"));
    }

    // 4.
    let expected = (400_0000 + 1_0000 * (v_set.len() as u64 - 1) + 1) * FRA;
    transfer_asset_batch_x(
        &v_set[0].keypair,
        &v_set.iter().skip(1).map(|v| v.pubkey).collect::<Vec<_>>(),
        None,
        expected,
        true,
        true,
    )
    .c(d!())?;

    // 5.
    sleep_n_block!(1.2);

    // 6.
    for v in v_set.iter().skip(1) {
        let actual = get_balance(&v.keypair).c(d!())?;
        alt!(
            actual > expected + TX_FEE_MIN || actual < expected,
            return Err(eg!("incorrect balance"))
        );
    }

    Ok(())
}
