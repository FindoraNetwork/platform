#![allow(clippy::field_reassign_with_default)]

#[cfg(not(feature = "abci_mock"))]
pub use real::gen_tendermint_attr;

#[cfg(feature = "abci_mock")]
pub use mocker::gen_tendermint_attr;

#[cfg(not(feature = "abci_mock"))]
mod real {
    use ledger::data_model::{Operation, Transaction, TxnSID};
    use serde::Serialize;
    use std::time::SystemTime;
    use tm_protos::{abci::*, libs::kv::*};
    use zei::xfr::structs::{XfrAmount, XfrAssetType};

    /// generate attr(tags) for index-ops of tendermint
    ///   - "tx.exist" => "y"
    ///   - "addr.from" => "Json<TagAttr>"
    ///   - "addr.to" => "Json<TagAttr>"
    ///   - "addr.from.<addr>" => "y"
    ///   - "addr.to.<addr>" => "y"
    pub fn gen_tendermint_attr(tx: &Transaction) -> Vec<Event> {
        let mut res = vec![];

        // index txs without block info
        let mut ev = Event::default();
        ev.r#type = String::from("tx");

        let mut kv = vec![Pair::default(), Pair::default()];
        kv[0].key = "prehash".as_bytes().to_vec();
        kv[0].value = hex::encode(tx.hash(TxnSID(0))).into_bytes();
        kv[1].key = "timestamp".as_bytes().to_vec();
        kv[1].value = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs()
            .to_string()
            .into_bytes();

        ev.attributes = kv;
        res.push(ev);

        let (from, to) = gen_tendermint_attr_addr(tx);

        if !from.is_empty() || !to.is_empty() {
            let mut ev = Event::default();
            ev.r#type = String::from("addr");

            let mut kv = vec![Pair::default(), Pair::default()];
            kv[0].key = "from".as_bytes().to_vec();
            kv[0].value = serde_json::to_vec(&from).unwrap();
            kv[1].key = "to".as_bytes().to_vec();
            kv[1].value = serde_json::to_vec(&to).unwrap();

            ev.attributes = kv;
            res.push(ev);

            macro_rules! index_addr {
                ($attr: expr, $ty: expr) => {
                    let kv = $attr
                        .into_iter()
                        .map(|i| {
                            let mut p = Pair::default();
                            p.key = i.addr.into_bytes();
                            p.value = "y".as_bytes().to_vec();
                            p
                        })
                        .collect::<Vec<_>>();

                    if !kv.is_empty() {
                        let mut ev = Event::default();
                        ev.r#type = String::from($ty);
                        ev.attributes = kv;
                        res.push(ev);
                    }
                };
            }

            index_addr!(from, "addr.from");
            index_addr!(to, "addr.to");
        }

        res
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
                        attr.addr =
                            globutils::wallet::public_key_to_bech32(&$data.pubkey);
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
    use ledger::data_model::Transaction;
    use tm_protos::abci::Event;

    pub fn gen_tendermint_attr(_tx: &Transaction) -> Vec<Event> {
        vec![Event::default()]
    }
}
