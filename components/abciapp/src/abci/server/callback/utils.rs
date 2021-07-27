#![allow(clippy::field_reassign_with_default)]

#[cfg(not(feature = "abci_mock"))]
pub use real::gen_tendermint_attr;

#[cfg(feature = "abci_mock")]
pub use mocker::gen_tendermint_attr;

#[cfg(not(feature = "abci_mock"))]
mod real {
    use abci::{Event, Pair};
    use ledger::data_model::{Operation, Transaction, TxnSID};
    use protobuf::RepeatedField;
    use serde::Serialize;
    use std::time::SystemTime;
    use zei::xfr::structs::{XfrAmount, XfrAssetType};

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

        if !from.is_empty() || !to.is_empty() {
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
                            attr.addr = wallet::public_key_to_bech32(&i.public_key);
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
                        attr.addr = wallet::public_key_to_bech32(&$data.pubkey);
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
}

#[cfg(feature = "abci_mock")]
mod mocker {
    use abci::Event;
    use ledger::data_model::Transaction;
    use protobuf::RepeatedField;

    pub fn gen_tendermint_attr(_tx: &Transaction) -> RepeatedField<Event> {
        RepeatedField::from_vec(vec![Event::new()])
    }
}
