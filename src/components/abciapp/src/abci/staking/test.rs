#![allow(missing_docs)]

use {
    finutils::txn_builder::{TransactionBuilder, TransferOperationBuilder},
    ledger::{
        data_model::{
            Transaction, TransferType, TxnEffect, TxoRef, ASSET_TYPE_FRA,
            BLACK_HOLE_PUBKEY, TX_FEE_MIN,
        },
        staking::{FF_PK_LIST, FRA_PRE_ISSUE_AMOUNT},
        store::LedgerState,
        utils::fra_gen_initial_tx,
    },
    noah::xfr::{
        asset_record::{open_blind_asset_record, AssetRecordType},
        structs::{AssetRecordTemplate, XfrAmount},
    },
    rand::random,
    rand_chacha::ChaChaRng,
    rand_core::SeedableRng,
    ruc::*,
    zei::{XfrKeyPair, XfrPublicKey},
};

#[test]
fn staking_block_rewards_rate() {
    pnk!(check_block_rewards_rate());
}

// 1. create a ledger instance
// 2. define and issue FRAs
// 3. transfer to some addresses of the 9 reserved accounts looply
// 4. check if the block rewards rate is correct per loop
fn check_block_rewards_rate() -> Result<()> {
    let mut ledger = LedgerState::tmp_ledger();
    let root_kp = XfrKeyPair::generate(&mut ChaChaRng::from_entropy());

    let tx = fra_gen_initial_tx(&root_kp);

    let effect = TxnEffect::compute_effect(tx).c(d!())?;
    let mut block = ledger.start_block().c(d!())?;
    ledger.apply_transaction(&mut block, effect).c(d!())?;
    ledger.finish_block(block).c(d!())?;

    // set a fake delegation amount
    *ledger.get_staking_mut().get_global_delegation_amount_mut() =
        FRA_PRE_ISSUE_AMOUNT / 200;

    let mut seq_id = 1;
    let mut prev_rate = [2, 1];
    for i in 1..200 {
        let tx = gen_transfer_tx(
            &ledger,
            &root_kp,
            &XfrPublicKey::from_noah(&FF_PK_LIST[random::<usize>() % FF_PK_LIST.len()])?,
            FRA_PRE_ISSUE_AMOUNT / 200,
            seq_id,
        )
        .c(d!())?;

        let effect = TxnEffect::compute_effect(tx).c(d!())?;
        let mut block = ledger.start_block().c(d!())?;
        ledger.apply_transaction(&mut block, effect).c(d!())?;
        ledger.finish_block(block).c(d!())?;

        {
            let rate = ledger.staking_get_block_rewards_rate();
            let rate = [rate[0], rate[1]];
            // max value: 105%
            assert!(rate[0] * 100 <= rate[1] * 105);
            // min value: 2%
            assert!(rate[0] * 100 >= rate[1] * 2);

            if 1 == i {
                assert_eq!(rate[0] * 100, rate[1] * 105);
            } else if 499 == i {
                assert_eq!(rate[0] * 100, rate[1] * 2);
            }

            // more locked percent, less return rate
            if rate[0] * 100 != rate[1] * 105 && rate[0] * 100 != rate[1] * 2 {
                assert!(rate[0] * prev_rate[1] < rate[1] * prev_rate[0]);
            }

            prev_rate = rate;
        }

        seq_id += 1;
    }

    Ok(())
}

fn gen_transfer_tx(
    la: &LedgerState,
    owner_kp: &XfrKeyPair,
    target_pk: &XfrPublicKey,
    am: u64,
    seq_id: u64,
) -> Result<Transaction> {
    let mut tx_builder = TransactionBuilder::from_seq_id(seq_id);

    let binding = XfrPublicKey::from_noah(&BLACK_HOLE_PUBKEY)?;
    let target_list = vec![(target_pk, am), (&binding, TX_FEE_MIN)];

    let mut trans_builder = TransferOperationBuilder::new();

    let mut am = target_list.iter().map(|(_, am)| *am).sum();
    let mut i_am;
    let utxos = la
        .get_owned_utxos(owner_kp.get_pk_ref())
        .c(d!())?
        .into_iter();

    for (sid, (utxo, owner_memo)) in utxos {
        if let XfrAmount::NonConfidential(n) = utxo.0.record.amount {
            alt!(n < am, i_am = n, i_am = am);
            am = am.saturating_sub(n);
        } else {
            continue;
        }

        open_blind_asset_record(
            &utxo.0.record.into_noah()?,
            &owner_memo,
            &owner_kp.into_noah()?,
        )
        .c(d!())
        .and_then(|ob| {
            trans_builder
                .add_input(TxoRef::Absolute(sid), ob, None, None, i_am)
                .c(d!())
        })?;

        alt!(0 == am, break);
    }

    if 0 != am {
        return Err(eg!("insufficient balance"));
    }

    let outputs = target_list.into_iter().map(|(pk, n)| {
        AssetRecordTemplate::with_no_asset_tracing(
            n,
            ASSET_TYPE_FRA,
            AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
            pk.into_noah().unwrap(),
        )
    });

    for output in outputs {
        trans_builder
            .add_output(&output, None, None, None)
            .c(d!())?;
    }

    let op = trans_builder
        .balance(None)
        .c(d!())?
        .create(TransferType::Standard)
        .c(d!())?
        .sign(owner_kp)
        .c(d!())?
        .transaction()
        .c(d!())?;

    tx_builder.add_operation(op);
    tx_builder.build_and_take_transaction()
}
