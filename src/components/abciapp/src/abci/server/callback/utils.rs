#![allow(clippy::field_reassign_with_default)]

use {
    abci::{Event, Pair},
    ledger::data_model::{Operation, Transaction, TxnSID},
    protobuf::RepeatedField,
    serde::Serialize,
    std::time::SystemTime,
    zei::noah_api::xfr::structs::{XfrAmount, XfrAssetType},
};

/// generate attr(tags) for index-ops of tendermint
///   - "tx.exist" => "y"
///   - "addr.from" => "Json<TagAttr>"
///   - "addr.to" => "Json<TagAttr>"
///   - "addr.from.<addr>" => "y"
///   - "addr.to.<addr>" => "y"
pub fn gen_tendermint_attr(tx: &Transaction) -> RepeatedField<Event> {
    let mut res = vec![];

    // index txs without block info
    let mut ev = Event::new();
    ev.set_field_type("tx".to_owned());

    let mut kv = vec![Pair::new(), Pair::new()];
    kv[0].set_key("prehash".as_bytes().to_vec());
    kv[0].set_value(hex::encode(tx.hash(TxnSID(0))).into_bytes());
    kv[1].set_key("timestamp".as_bytes().to_vec());
    kv[1].set_value(
        SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs()
            .to_string()
            .into_bytes(),
    );

    ev.set_attributes(RepeatedField::from_vec(kv));
    res.push(ev);

    let (from, to) = gen_tendermint_attr_addr(tx);
    let (nullifiers, commitments) = gen_tendermint_attr_anon(tx);

    if !from.is_empty()
        || !to.is_empty()
        || !nullifiers.is_empty()
        || !commitments.is_empty()
    {
        let mut ev = Event::new();
        ev.set_field_type("addr".to_owned());

        let mut kv = vec![Pair::new(), Pair::new()];
        kv[0].set_key("from".as_bytes().to_vec());
        kv[0].set_value(serde_json::to_vec(&from).unwrap());
        kv[1].set_key("to".as_bytes().to_vec());
        kv[1].set_value(serde_json::to_vec(&to).unwrap());

        ev.set_attributes(RepeatedField::from_vec(kv));
        res.push(ev);

        macro_rules! index_addr {
            ($attr: expr, $ty: expr) => {
                let kv = $attr
                    .into_iter()
                    .map(|i| {
                        let mut p = Pair::new();
                        p.set_key(i.addr.into_bytes());
                        p.set_value("y".as_bytes().to_vec());
                        p
                    })
                    .collect::<Vec<_>>();

                if !kv.is_empty() {
                    let mut ev = Event::new();
                    ev.set_field_type($ty.to_owned());
                    ev.set_attributes(RepeatedField::from_vec(kv));
                    res.push(ev);
                }
            };
        }

        index_addr!(from, "addr.from");
        index_addr!(to, "addr.to");
        index_addr!(nullifiers, "nullifier.used");
        index_addr!(commitments, "commitment.created");
    }

    RepeatedField::from_vec(res)
}

// collect informations of inputs and outputs
// # return: ([from ...], [to ...])
fn gen_tendermint_attr_addr(tx: &Transaction) -> (Vec<TagAttr>, Vec<TagAttr>) {
    tx.body
        .operations
        .iter()
        .fold((vec![], vec![]), |mut base, new| {
            macro_rules! append_attr {
                // trasfer\bind\release
                ($data: expr, $direction: tt, $idx: tt) => {
                    $data.body.transfer.$direction.iter().for_each(|i| {
                        let mut attr = TagAttr::default();
                        attr.addr =
                            globutils::wallet::public_key_to_bech32(&i.public_key);
                        if let XfrAssetType::NonConfidential(ty) = i.asset_type {
                            attr.asset_type = Some(hex::encode(&ty.0[..]));
                        }
                        if let XfrAmount::NonConfidential(am) = i.amount {
                            attr.asset_amount = Some(am);
                        }
                        base.$idx.push(attr);
                    });
                };
                // define\issue\AIR\memo
                ($data: expr) => {
                    let mut attr = TagAttr::default();
                    attr.addr = globutils::wallet::public_key_to_bech32(&$data.pubkey);
                    base.0.push(attr);
                };
            }

            match new {
                Operation::TransferAsset(d) => {
                    append_attr!(d, inputs, 0);
                    append_attr!(d, outputs, 1);
                }
                Operation::DefineAsset(d) => {
                    append_attr!(d);
                }
                Operation::IssueAsset(d) => {
                    append_attr!(d);
                }
                Operation::UpdateMemo(d) => {
                    append_attr!(d);
                }
                Operation::BarToAbar(d) => {
                    let mut attr = TagAttr::default();
                    attr.addr = globutils::wallet::public_key_to_bech32(
                        &d.input_record().public_key,
                    );
                    base.0.push(attr);
                }
                Operation::AbarToBar(d) => {
                    let mut attr = TagAttr::default();
                    attr.addr = globutils::wallet::public_key_to_bech32(
                        &d.note.get_public_key(),
                    );
                    base.1.push(attr);
                }
                _ => {}
            }

            base
        })
}

fn gen_tendermint_attr_anon(tx: &Transaction) -> (Vec<TagAttr>, Vec<TagAttr>) {
    tx.body
        .operations
        .iter()
        .fold((vec![], vec![]), |mut base, op| {
            match op {
                Operation::BarToAbar(d) => {
                    let mut attr = TagAttr::default();
                    attr.addr = globutils::wallet::commitment_to_base58(
                        &d.output_record().commitment,
                    );
                    base.1.push(attr);
                }
                Operation::AbarToBar(d) => {
                    let mut attr = TagAttr::default();
                    attr.addr =
                        globutils::wallet::nullifier_to_base58(&d.note.get_input());
                    base.0.push(attr);
                }
                Operation::TransferAnonAsset(d) => {
                    for ix in &d.note.body.inputs {
                        let mut attr = TagAttr::default();
                        attr.addr = globutils::wallet::nullifier_to_base58(ix);
                        base.0.push(attr);
                    }
                    for ox in &d.note.body.outputs {
                        let mut attr = TagAttr::default();
                        attr.addr =
                            globutils::wallet::commitment_to_base58(&ox.commitment);
                        base.1.push(attr);
                    }
                }
                _ => {}
            }

            base
        })
}

#[derive(Serialize, Default)]
struct TagAttr {
    // FRA address
    addr: String,
    // hex.encode(asset_type)
    asset_type: Option<String>,
    asset_amount: Option<u64>,
}
