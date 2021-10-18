//!
//! # init
//!
//! Initialize a new network environment.
//!

pub mod i_testing;

use super::*;

pub fn init(mut interval: u64, skip_validator: bool, is_mainnet: bool) -> Result<()> {
    if 0 == interval {
        interval = BLOCK_INTERVAL;
    }

    if !skip_validator {
        println!(">>> Set initial validator set ...");
        common::set_initial_validators().c(d!())?;
    }

    if !is_mainnet {
        let root_kp =
            wallet::restore_keypair_from_mnemonic_default(ROOT_MNEMONIC).c(d!())?;
        println!(">>> Block interval: {} seconds", interval);

        println!(">>> Define and issue FRA ...");
        common::utils::send_tx(&fra_gen_initial_tx(&root_kp)).c(d!())?;

        println!(">>> Wait 1.5 block ...");
        sleep_n_block!(1.5, interval);

        if skip_validator {
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
        common::utils::transfer_batch(&root_kp, target_list, None, false, false)
            .c(d!())?;
    }

    println!(">>> Wait 5 blocks ...");
    sleep_n_block!(5, interval);

    println!(">>> Propose self-delegations ...");
    for (i, v) in VALIDATOR_LIST.values().enumerate() {
        delegate::gen_tx(&v.name, (800_0000 + i as u64 * 1_0000) * FRA, &v.name)
            .c(d!())
            .and_then(|tx| common::utils::send_tx(&tx).c(d!()))?;
    }

    println!(">>> Init work done !");

    println!(">>> Start running integration tests ...");

    println!(">>> Wait 5 block ...");
    sleep_n_block!(5);

    i_testing::run_all().c(d!())
}
