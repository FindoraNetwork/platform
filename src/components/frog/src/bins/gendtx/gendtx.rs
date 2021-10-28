use {
    finutils::txn_builder::{TransactionBuilder, TransferOperationBuilder},
    frog::UtxoMap,
    globutils::wallet,
    ledger::{
        data_model::{
            Operation, Transaction, TransferType, TxoRef, TxoSID, Utxo, ASSET_TYPE_FRA,
            BLACK_HOLE_PUBKEY, BLACK_HOLE_PUBKEY_STAKING, TX_FEE_MIN,
        },
        staking::{check_delegation_amount, init::get_cfg_data, FRA},
    },
    rand::random,
    ruc::*,
    std::{collections::BTreeMap, env::args, fs},
    zei::xfr::{
        asset_record::{open_blind_asset_record, AssetRecordType},
        sig::XfrKeyPair,
        structs::{AssetRecordTemplate, OwnerMemo},
    },
};

fn main() {
    for tx in pnk!(gen_txs()).iter() {
        println!("{}", serde_json::to_string(tx).unwrap());
    }
}

fn gen_txs() -> Result<Vec<Transaction>> {
    let seq_id = include_str!("seq_id.str").parse::<u64>().c(d!())?;
    let utxo_map =
        serde_json::from_slice::<UtxoMap>(include_bytes!("utxo.map")).c(d!())?;
    let addr_map = get_cfg_data()
        .c(d!())?
        .valiators
        .into_iter()
        .map(|v| (wallet::public_key_from_base64(&v.id).unwrap(), v.td_addr))
        .collect::<BTreeMap<_, _>>();
    let keypairs = args()
        .nth(1)
        .c(d!("Usage: ./gendtx <PATH-TO-KEY-FILE>"))
        .and_then(|file| fs::read_to_string(&file).c(d!()))
        .and_then(|c| {
            c.lines()
                .map(|l| {
                    let msg = format!("Invalid key: {}", l);
                    wallet::restore_keypair_from_seckey_base64(l).c(d!(msg))
                })
                .collect::<Result<Vec<_>>>()
        })?;

    let keys = keypairs
        .iter()
        .map(|kp| (kp.get_pk(), kp))
        .collect::<BTreeMap<_, _>>();

    let kl = keys.len();
    let aml = addr_map.len();
    let uml = utxo_map.len();
    if kl != aml || kl != uml {
        let msg = format!(
            "The number of validators({}) and keys({}) are inconsistent!",
            aml, kl
        );
        return Err(eg!(msg));
    }

    let mut res = vec![];
    for (((pk0, utxos), (pk1, kp)), (pk2, td_addr)) in utxo_map
        .into_iter()
        .zip(keys.into_iter())
        .zip(addr_map.into_iter())
    {
        // we use BTreeMap, so they should be equal
        if pk0 != pk2 || pk1 != pk2 {
            let msg = format!(
                "'{}' can not be found in the registered key list!",
                serde_json::to_string(kp.get_sk_ref())
                    .c(d!())?
                    .trim_matches(|c| c == '"')
            );
            return Err(eg!(msg));
        }

        let amount = (379_8361 + random::<u64>() % 40_0000) * FRA;
        check_delegation_amount(amount, false).c(d!())?;

        let mut builder = TransactionBuilder::from_seq_id(seq_id);
        gen_transfer_op(kp, utxos, amount)
            .c(d!())
            .map(|principal_op| {
                builder.add_operation(principal_op);
                builder.add_operation_delegation(kp, amount, td_addr);
            })?;
        res.push(builder.take_transaction());
    }

    Ok(res)
}

fn gen_transfer_op(
    owner_kp: &XfrKeyPair,
    utxos: BTreeMap<TxoSID, (Utxo, Option<OwnerMemo>)>,
    amount: u64,
) -> Result<Operation> {
    let target_list = vec![
        (&*BLACK_HOLE_PUBKEY_STAKING, amount),
        // add fee at the last position
        (&*BLACK_HOLE_PUBKEY, TX_FEE_MIN),
    ];

    let mut trans_builder = TransferOperationBuilder::new();

    let mut am = target_list.iter().map(|(_, am)| *am).sum();
    let mut i_am;

    for (sid, (utxo, owner_memo)) in utxos {
        let oar =
            open_blind_asset_record(&utxo.0.record, &owner_memo, owner_kp).c(d!())?;

        if ASSET_TYPE_FRA == oar.asset_type {
            alt!(oar.amount < am, i_am = oar.amount, i_am = am);
            am -= i_am;
            trans_builder
                .add_input(TxoRef::Absolute(sid), oar, None, None, i_am)
                .c(d!())?;
        }

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
            *pk,
        )
    });

    for output in outputs {
        trans_builder
            .add_output(&output, None, None, None)
            .c(d!())?;
    }

    trans_builder
        .balance(Some(
            AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
        ))
        .c(d!())?
        .create(TransferType::Standard)
        .c(d!())?
        .sign(owner_kp)
        .c(d!())?
        .transaction()
        .c(d!())
}
