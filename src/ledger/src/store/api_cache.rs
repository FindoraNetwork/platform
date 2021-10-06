//!
//! # Cached data for APIs
//!

use crate::{
    data_model::{
        AssetTypeCode, DefineAsset, IssueAsset, IssuerPublicKey, Operation, Transaction,
        TxOutput, TxnIDHash, TxnSID, TxoSID, XfrAddress,
    },
    staking::{
        ops::mint_fra::MintEntry, Amount, BlockHeight, DelegationRwdDetail,
        CHAN_D_AMOUNT_HIST, CHAN_D_RWD_HIST, CHAN_GLOB_RATE_HIST, CHAN_V_SELF_D_HIST,
    },
};
use fbnc::{new_mapx, Mapx};
use globutils::wallet;
use ruc::*;
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use zei::xfr::{sig::XfrPublicKey, structs::OwnerMemo};

type Issuances = Vec<(TxOutput, Option<OwnerMemo>)>;

/// Used in APIs
#[derive(Clone, Deserialize, Serialize)]
pub struct ApiCache {
    /// Set of transactions related to a ledger address
    pub related_transactions: Mapx<XfrAddress, HashSet<TxnSID>>,
    /// Set of transfer transactions related to an asset code
    pub related_transfers: Mapx<AssetTypeCode, HashSet<TxnSID>>,
    /// List of claim transactions related to a ledger address
    pub claim_hist_txns: Mapx<XfrAddress, Vec<TxnSID>>,
    /// Payments from coinbase
    pub coinbase_oper_hist: Mapx<XfrAddress, Vec<(BlockHeight, MintEntry)>>,
    /// Created assets
    pub created_assets: Mapx<IssuerPublicKey, Vec<DefineAsset>>,
    /// issuance mapped by public key
    pub issuances: Mapx<IssuerPublicKey, Issuances>,
    /// issuance mapped by token code
    pub token_code_issuances: Mapx<AssetTypeCode, Issuances>,
    /// used in confidential tx
    pub owner_memos: Mapx<TxoSID, OwnerMemo>,
    /// ownship of txo
    pub utxos_to_map_index: Mapx<TxoSID, XfrAddress>,
    /// txo(spent, unspent) to authenticated txn (sid, hash)
    pub txo_to_txnid: Mapx<TxoSID, TxnIDHash>,
    /// txn sid to txn hash
    pub txn_sid_to_hash: Mapx<TxnSID, String>,
    /// txn hash to txn sid
    pub txn_hash_to_sid: Mapx<String, TxnSID>,
    /// global rate history
    pub staking_global_rate_hist: Mapx<BlockHeight, [u128; 2]>,
    /// - self-delegation amount history
    ///   - `NonConfidential` FRAs amount
    ///   - only valid for validators
    pub staking_self_delegation_hist: Mapx<XfrPublicKey, Mapx<BlockHeight, Amount>>,
    /// - delegation amount per block height
    /// - only valid for a validator
    pub staking_delegation_amount_hist: Mapx<XfrPublicKey, Mapx<BlockHeight, Amount>>,
    /// rewards history, used on some pulic nodes, such as fullnode
    pub staking_delegation_rwd_hist:
        Mapx<XfrPublicKey, Mapx<BlockHeight, DelegationRwdDetail>>,
}

impl ApiCache {
    pub(crate) fn new(prefix: &str) -> Self {
        ApiCache {
            related_transactions: new_mapx!(format!(
                "api_cache/{}related_transactions",
                prefix
            )),
            related_transfers: new_mapx!(format!(
                "api_cache/{}related_transfers",
                prefix
            )),
            claim_hist_txns: new_mapx!(format!("api_cache/{}claim_hist_txns", prefix)),
            coinbase_oper_hist: new_mapx!(format!(
                "api_cache/{}coinbase_oper_hist",
                prefix
            )),
            created_assets: new_mapx!(format!("api_cache/{}created_assets", prefix)),
            issuances: new_mapx!(format!("api_cache/{}issuances", prefix)),
            token_code_issuances: new_mapx!(format!(
                "api_cache/{}token_code_issuances",
                prefix
            )),
            owner_memos: new_mapx!(format!("api_cache/{}owner_memos", prefix)),
            utxos_to_map_index: new_mapx!(format!(
                "api_cache/{}utxos_to_map_index",
                prefix
            )),
            txo_to_txnid: new_mapx!(format!("api_cache/{}txo_to_txnid", prefix)),
            txn_sid_to_hash: new_mapx!(format!("api_cache/{}txn_sid_to_hash", prefix)),
            txn_hash_to_sid: new_mapx!(format!("api_cache/{}txn_hash_to_sid", prefix)),
            staking_global_rate_hist: new_mapx!(format!(
                "api_cache/{}staking_global_rate_hist",
                prefix
            )),
            staking_self_delegation_hist: new_mapx!(format!(
                "api_cache/{}staking_self_delegation_hist",
                prefix
            )),
            staking_delegation_amount_hist: new_mapx!(format!(
                "api_cache/{}staking_delegation_amount_hist",
                prefix
            )),
            staking_delegation_rwd_hist: new_mapx!(format!(
                "api_cache/{}staking_delegation_rwd_hist",
                prefix
            )),
        }
    }

    /// Add created asset
    #[inline(always)]
    pub fn add_created_asset(&mut self, creation: &DefineAsset) {
        let issuer = creation.pubkey;
        #[allow(unused_mut)]
        let mut set = self.created_assets.entry(issuer).or_insert_with(Vec::new);

        set.push(creation.clone());
        set.sort_by_key(|i| i.pubkey);
        set.dedup_by_key(|i| i.body.asset.code);
    }

    /// Cache issuance records
    pub fn cache_issuance(&mut self, issuance: &IssueAsset) {
        let new_records = issuance.body.records.to_vec();

        macro_rules! save_issuance {
            ($maps: tt, $key: tt) => {
                #[allow(unused_mut)]
                let mut records = $maps.entry($key).or_insert_with(Vec::new);
                records.extend_from_slice(&new_records);
            };
        }

        let key_issuances = &mut self.issuances;
        let pubkey = issuance.pubkey;
        save_issuance!(key_issuances, pubkey);

        let token_issuances = &mut self.token_code_issuances;
        let token_code = issuance.body.code;
        save_issuance!(token_issuances, token_code);
    }

    /// Cache history style data
    pub fn cache_hist_data(&mut self) {
        CHAN_GLOB_RATE_HIST.1.lock().try_iter().for_each(|(h, r)| {
            self.staking_global_rate_hist.insert(h, r);
        });

        CHAN_V_SELF_D_HIST
            .1
            .lock()
            .try_iter()
            .for_each(|(pk, h, r)| {
                self.staking_self_delegation_hist
                    .entry(pk)
                    .or_insert(new_mapx!(format!(
                        "staking_self_delegation_hist_subdata/{}",
                        wallet::public_key_to_base64(&pk)
                    )))
                    .insert(h, r);
            });

        CHAN_D_AMOUNT_HIST
            .1
            .lock()
            .try_iter()
            .for_each(|(pk, h, r)| {
                self.staking_delegation_amount_hist
                    .entry(pk)
                    .or_insert(new_mapx!(format!(
                        "staking_delegation_amount_hist_subdata/{}",
                        wallet::public_key_to_base64(&pk)
                    )))
                    .insert(h, r);
            });

        CHAN_D_RWD_HIST.1.lock().try_iter().for_each(|(pk, h, r)| {
            #[allow(unused_mut)]
            let mut dd =
                self.staking_delegation_rwd_hist
                    .entry(pk)
                    .or_insert(new_mapx!(format!(
                        "staking_delegation_rwd_hist_subdata/{}",
                        wallet::public_key_to_base64(&pk)
                    )));
            let mut dd = dd.entry(h).or_insert_with(DelegationRwdDetail::default);

            dd.block_height = r.block_height;
            dd.amount += r.amount;
            dd.penalty_amount += r.penalty_amount;

            alt!(0 < r.bond, dd.bond = r.bond);
            alt!(r.return_rate.is_some(), dd.return_rate = r.return_rate);
            alt!(
                r.commission_rate.is_some(),
                dd.commission_rate = r.commission_rate
            );
            alt!(
                r.global_delegation_percent.is_some(),
                dd.global_delegation_percent = r.global_delegation_percent
            );
        });
    }
}

/// An xfr address is related to a transaction if it is one of the following:
/// 1. Owner of a transfer output
/// 2. Transfer signer (owner of input or co-signer)
/// 3. Signer of a an issuance txn
/// 4. Signer of a kv_update txn
/// 5. Signer of a memo_update txn
pub fn get_related_addresses<F>(
    txn: &Transaction,
    mut classify: F,
) -> HashSet<XfrAddress>
where
    F: FnMut(&Operation),
{
    let mut related_addresses = HashSet::new();

    macro_rules! staking_gen {
        ($op: expr) => {
            $op.get_related_pubkeys().into_iter().for_each(|pk| {
                related_addresses.insert(XfrAddress { key: pk });
            });
        };
    }

    for op in &txn.body.operations {
        classify(op);
        match op {
            Operation::UpdateStaker(i) => staking_gen!(i),
            Operation::Delegation(i) => staking_gen!(i),
            Operation::UnDelegation(i) => staking_gen!(i),
            Operation::Claim(i) => staking_gen!(i),
            Operation::UpdateValidator(i) => staking_gen!(i),
            Operation::Governance(i) => staking_gen!(i),
            Operation::FraDistribution(i) => staking_gen!(i),
            Operation::MintFra(i) => staking_gen!(i),

            Operation::ConvertAccount(i) => {
                related_addresses.insert(XfrAddress {
                    key: i.get_related_address(),
                });
            }
            Operation::TransferAsset(transfer) => {
                for input in transfer.body.transfer.inputs.iter() {
                    related_addresses.insert(XfrAddress {
                        key: input.public_key,
                    });
                }

                for output in transfer.body.transfer.outputs.iter() {
                    related_addresses.insert(XfrAddress {
                        key: output.public_key,
                    });
                }
            }
            Operation::IssueAsset(issue_asset) => {
                related_addresses.insert(XfrAddress {
                    key: issue_asset.pubkey.key,
                });
            }
            Operation::DefineAsset(define_asset) => {
                related_addresses.insert(XfrAddress {
                    key: define_asset.pubkey.key,
                });
            }
            Operation::UpdateMemo(update_memo) => {
                related_addresses.insert(XfrAddress {
                    key: update_memo.pubkey,
                });
            }
        }
    }
    related_addresses
}

/// Returns the set of nonconfidential assets transferred in a transaction.
pub fn get_transferred_nonconfidential_assets(
    txn: &Transaction,
) -> HashSet<AssetTypeCode> {
    let mut transferred_assets = HashSet::new();
    for op in &txn.body.operations {
        if let Operation::TransferAsset(transfer) = op {
            for input in transfer.body.transfer.inputs.iter() {
                if let Some(asset_type) = input.asset_type.get_asset_type() {
                    transferred_assets.insert(AssetTypeCode { val: asset_type });
                }
            }
        }
    }
    transferred_assets
}
