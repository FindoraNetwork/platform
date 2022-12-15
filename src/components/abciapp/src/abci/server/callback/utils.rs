#![allow(clippy::field_reassign_with_default)]

use {
    ledger::data_model::{Operation, Transaction, TxnSID},
    serde::Serialize,
    std::time::SystemTime,
    tendermint_proto::abci::{Event, EventAttribute},
    zei::xfr::structs::{XfrAmount, XfrAssetType},
};

/// generate attr(tags) for index-ops of tendermint
///   - "tx.exist" => "y"
///   - "addr.from" => "Json<TagAttr>"
///   - "addr.to" => "Json<TagAttr>"
///   - "addr.from.<addr>" => "y"
///   - "addr.to.<addr>" => "y"
pub fn gen_tendermint_attr(tx: &Transaction) -> Vec<Event> {
    let mut res = vec![];

    let attr_prehash = EventAttribute {
        key: "prehash".as_bytes().into(),
        value: hex::encode(tx.hash(TxnSID(0))).into_bytes().into(),
        index: true,
    };
    let attr_timestamp = EventAttribute {
        key: "timestamp".as_bytes().into(),
        value: SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs()
            .to_string()
            .into_bytes()
            .into(),
        index: true,
    };

    res.push(Event {
        r#type: "tx".to_owned(),
        attributes: vec![attr_prehash, attr_timestamp],
    });

    let (from, to) = gen_tendermint_attr_addr(tx);

    if !from.is_empty() || !to.is_empty() {
        let attr_from = EventAttribute {
            key: "from".as_bytes().into(),
            value: serde_json::to_vec(&from).unwrap().into(),
            index: true,
        };
        let attr_to = EventAttribute {
            key: "to".as_bytes().into(),
            value: serde_json::to_vec(&to).unwrap().into(),
            index: true,
        };

        res.push(Event {
            r#type: "addr".into(),
            attributes: vec![attr_from, attr_to],
        });

        macro_rules! index_addr {
            ($attr: expr, $ty: expr) => {
                let kv = $attr
                    .into_iter()
                    .map(|i| EventAttribute {
                        key: i.addr.into_bytes().into(),
                        value: "y".as_bytes().into(),
                        index: true,
                    })
                    .collect::<Vec<_>>();

                if !kv.is_empty() {
                    res.push(Event {
                        r#type: $ty.into(),
                        attributes: kv,
                    });
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
