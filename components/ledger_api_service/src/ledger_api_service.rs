#![deny(warnings)]
#![allow(clippy::needless_borrow)]

extern crate actix_rt;
extern crate actix_web;
extern crate ledger;
extern crate serde_json;

use actix_cors::Cors;
use actix_web::{dev, error, middleware, web, App, HttpResponse, HttpServer};
use ledger::staking::{DelegationRwdDetail, TendermintAddr};
use ledger::{
    data_model::*,
    staking::{DelegationState, Staking, UNBOND_BLOCK_CNT},
    store::LedgerAccess,
};
use log::info;
use log::warn;
use parking_lot::RwLock;
use ruc::*;
use serde::Serialize;
use serde_derive::Deserialize;
use std::{collections::BTreeMap, mem, sync::Arc};
use utils::{HashOf, NetworkRoute, SignatureOf};
use zei::xfr::{sig::XfrPublicKey, structs::OwnerMemo};

pub struct RestfulApiService {
    web_runtime: actix_rt::SystemRunner,
}

// Ping route to check for liveness of API
#[allow(clippy::unnecessary_wraps)]
async fn ping() -> actix_web::Result<String> {
    Ok("success".into())
}

/// Returns the git commit hash and commit date of this build
#[allow(clippy::unnecessary_wraps)]
async fn version() -> actix_web::Result<String> {
    Ok(format!(
        "Build: {} {}",
        option_env!("VERGEN_SHA_EXTERN").unwrap_or(env!("VERGEN_SHA")),
        env!("VERGEN_BUILD_DATE")
    ))
}

// Future refactor:
// Merge query functions
//
// Query functions for LedgerAccess are very similar, especially these three:
//   query_asset
//   query_policy
//   query_contract
// If we add more functions with the similar pattern, it will be good to merge them

pub async fn query_utxo<LA>(
    data: web::Data<Arc<RwLock<LA>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<AuthenticatedUtxo>>
where
    LA: LedgerAccess,
{
    let reader = data.read();
    if let Ok(txo_sid) = info.parse::<u64>() {
        if let Some(txo) = reader.get_utxo(TxoSID(txo_sid)) {
            Ok(web::Json(txo))
        } else {
            Err(actix_web::error::ErrorNotFound(
                "Specified txo does not currently exist.",
            ))
        }
    } else {
        Err(actix_web::error::ErrorBadRequest(
            "Invalid txo sid encoding",
        ))
    }
}

pub async fn query_asset_issuance_num<LA>(
    data: web::Data<Arc<RwLock<LA>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<u64>>
where
    LA: LedgerAccess,
{
    let reader = data.read();
    if let Ok(token_code) = AssetTypeCode::new_from_base64(&*info) {
        if let Some(iss_num) = reader.get_issuance_num(&token_code) {
            Ok(web::Json(iss_num))
        } else {
            Err(actix_web::error::ErrorNotFound(
                "Specified asset definition does not currently exist.",
            ))
        }
    } else {
        Err(actix_web::error::ErrorBadRequest(
            "Invalid asset definition encoding.",
        ))
    }
}

pub async fn query_utxos<LA>(
    data: web::Data<Arc<RwLock<LA>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<Vec<Option<AuthenticatedUtxo>>>>
where
    LA: LedgerAccess,
{
    let sid_list = info
        .as_ref()
        .split(',')
        .map(|i| {
            i.parse::<u64>()
                .map(TxoSID)
                .map_err(actix_web::error::ErrorBadRequest)
        })
        .collect::<actix_web::Result<Vec<_>, actix_web::error::Error>>()?;

    let reader = data.read();

    if sid_list.len() > 10 || sid_list.is_empty() {
        return Err(actix_web::error::ErrorBadRequest("Invalid Query List"));
    }

    Ok(web::Json(reader.get_utxos(sid_list.as_slice())))
}

pub async fn query_asset<LA>(
    data: web::Data<Arc<RwLock<LA>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<AssetType>>
where
    LA: LedgerAccess,
{
    let reader = data.read();
    if let Ok(token_code) = AssetTypeCode::new_from_base64(&*info) {
        if let Some(asset) = reader.get_asset_type(&token_code) {
            Ok(web::Json(asset.clone()))
        } else {
            Err(actix_web::error::ErrorNotFound(
                "Specified asset definition does not currently exist.",
            ))
        }
    } else {
        Err(actix_web::error::ErrorBadRequest(
            "Invalid asset definition encoding.",
        ))
    }
}

pub async fn query_txn<LA>(
    data: web::Data<Arc<RwLock<LA>>>,
    info: web::Path<String>,
) -> actix_web::Result<String>
where
    LA: LedgerAccess,
{
    let reader = data.read();
    if let Ok(txn_sid) = info.parse::<usize>() {
        if let Some(mut txn) = reader.get_transaction(TxnSID(txn_sid)) {
            txn.finalized_txn.set_txo_id();
            Ok(serde_json::to_string(&txn)?)
        } else {
            Err(actix_web::error::ErrorNotFound(
                "Specified transaction does not exist.",
            ))
        }
    } else {
        Err(actix_web::error::ErrorBadRequest(
            "Invalid txn sid encoding.",
        ))
    }
}

pub async fn query_public_key<LA>(
    data: web::Data<Arc<RwLock<LA>>>,
) -> web::Json<XfrPublicKey>
where
    LA: LedgerAccess,
{
    let reader = data.read();
    web::Json(*reader.public_key())
}

#[allow(clippy::type_complexity)]
pub async fn query_global_state<LA>(
    data: web::Data<Arc<RwLock<LA>>>,
) -> web::Json<(
    HashOf<Option<StateCommitmentData>>,
    u64,
    SignatureOf<(HashOf<Option<StateCommitmentData>>, u64)>,
)>
where
    LA: LedgerAccess,
{
    let reader = data.read();
    let (hash, seq_id) = reader.get_state_commitment();
    let sig = reader.sign_message(&(hash.clone(), seq_id));
    web::Json((hash, seq_id, sig))
}

pub async fn query_global_state_version<LA>(
    data: web::Data<Arc<RwLock<LA>>>,
    version: web::Path<u64>,
) -> web::Json<Option<HashOf<Option<StateCommitmentData>>>>
where
    LA: LedgerAccess,
{
    let reader = data.read();
    let hash = reader.get_state_commitment_at_block_height(*version);
    web::Json(hash)
}

async fn query_blocks_since<LA>(
    data: web::Data<Arc<RwLock<LA>>>,
    block_id: web::Path<usize>,
) -> web::Json<Vec<(usize, Vec<FinalizedTransaction>)>>
where
    LA: LedgerAccess,
{
    let reader = data.read();
    let mut ret = Vec::new();
    let blk_cnt = reader.get_block_count();

    // upper limit: 1000 txns of blocks per query
    const TXNS_TO_CUTOFF: usize = 1000;
    let mut txn_cnt = 0;
    for ix in block_id.into_inner()..blk_cnt {
        let sid = BlockSID(ix);
        if let Some(authenticated_block) = reader.get_block(sid) {
            txn_cnt += authenticated_block.block.txns.len();
            ret.push((sid.0, authenticated_block.block.txns.clone()));
        } else {
            warn!("query_blocks_since failed, range= [{}, {})", ix, blk_cnt);
            ret.clear();
            break;
        }
        // stop adding more blocks when reaches upper limit
        if txn_cnt >= TXNS_TO_CUTOFF {
            break;
        }
    }
    web::Json(ret)
}

async fn query_block_log<LA>(
    data: web::Data<Arc<RwLock<LA>>>,
) -> impl actix_web::Responder
where
    LA: LedgerAccess,
{
    let reader = data.read();
    let mut res = String::new();
    res.push_str("<html><head><META HTTP-EQUIV=\"Pragma\" CONTENT=\"no-cache\"></head><body><table border=\"1\">");
    res.push_str("<tr>");
    res.push_str("<th>Block ID</th>");
    res.push_str("<th>Transactions</th>");
    res.push_str("</tr>");
    for ix in 0..reader.get_block_count() {
        let authenticated_block = reader.get_block(BlockSID(ix)).unwrap();
        res.push_str("<tr>");

        res.push_str(&format!("<td>{}</td>", ix));

        res.push_str("<td><table border=\"1\">");
        res.push_str("<tr>");
        res.push_str("<th>TXN ID</th>");
        res.push_str("<th>merkle id</th>");
        res.push_str("<th>Operations</th>");
        res.push_str("</tr>");

        for txn in authenticated_block.block.txns.iter() {
            res.push_str("<tr>");
            res.push_str(&format!("<td>{}</td>", txn.tx_id.0));
            res.push_str(&format!("<td>{}</td>", txn.merkle_id));
            res.push_str("<td>");
            res.push_str("<table border=\"1\">");
            for op in txn.txn.body.operations.iter() {
                res.push_str(&format!(
                    "<tr><td><pre>{}</pre></td></tr>",
                    serde_json::to_string_pretty(&op).unwrap()
                ));
            }
            res.push_str("</table></td>");
            res.push_str("</tr>");
        }

        res.push_str("</table></td></tr>");
    }
    res.push_str("</table></body></html>");
    HttpResponse::Ok()
        .content_type("text/html; charset=utf-8")
        .body(res)
}

async fn query_utxo_map<LA>(
    data: web::Data<Arc<RwLock<LA>>>,
) -> actix_web::Result<String>
where
    LA: LedgerAccess,
{
    let mut reader = data.write();

    let vec = reader.serialize_utxo_map();
    Ok(serde_json::to_string(&vec)?)
}

async fn query_utxo_map_checksum<LA>(
    data: web::Data<Arc<RwLock<LA>>>,
    info: web::Path<String>,
) -> actix_web::Result<String>
where
    LA: LedgerAccess,
{
    if let Ok(version) = info.parse::<u64>() {
        let reader = data.read();

        if let Some(vec) = reader.get_utxo_checksum(version) {
            Ok(serde_json::to_string(&vec)?)
        } else {
            Err(actix_web::error::ErrorNotFound(
                "That version is unavailable.",
            ))
        }
    } else {
        Err(actix_web::error::ErrorBadRequest(
            "Invalid version encoding.",
        ))
    }
}

#[allow(unused)]
fn parse_blocks(block_input: String) -> Option<Vec<usize>> {
    let blocks = block_input.split(',');
    let mut result = Vec::new();

    for block_str in blocks {
        if let Ok(block_usize) = block_str.parse::<usize>() {
            result.push(block_usize);
        } else {
            return None;
        }
    }

    Some(result)
}

// query current validator list,
// validtors who have not completed self-deletagion will be filtered out.
#[allow(unused)]
async fn query_validators<LA>(
    data: web::Data<Arc<RwLock<LA>>>,
) -> actix_web::Result<web::Json<ValidatorList>>
where
    LA: LedgerAccess,
{
    let read = data.read();
    let staking = read.get_staking();

    if let Some(validator_data) = staking.validator_get_current() {
        let validators = validator_data.get_validator_addr_map();
        let validators_list = validators
            .iter()
            .flat_map(|(tendermint_addr, pk)| {
                validator_data.get_powered_validator_by_id(pk).map(|v| {
                    let rank = if v.td_power == 0 {
                        validator_data.body.len()
                    } else {
                        let mut power_list = validator_data
                            .body
                            .values()
                            .map(|v| v.td_power)
                            .collect::<Vec<_>>();
                        power_list.sort_unstable();
                        power_list.len() - power_list.binary_search(&v.td_power).unwrap()
                    };
                    Validator::new(
                        tendermint_addr.clone(),
                        Staking::get_block_rewards_rate(&*read),
                        rank as u64,
                        staking.delegation_has_addr(&pk),
                        &v,
                    )
                })
            })
            .collect();
        return Ok(web::Json(ValidatorList::new(
            staking.cur_height() as u64,
            validators_list,
        )));
    };

    Ok(web::Json(ValidatorList::new(0, vec![])))
}

#[derive(Deserialize, Debug)]
struct DelegationRwdQueryParams {
    address: String,
    height: u64,
}

async fn get_delegation_reward<SA>(
    data: web::Data<Arc<RwLock<SA>>>,
    web::Query(info): web::Query<DelegationRwdQueryParams>,
) -> actix_web::Result<web::Json<Vec<DelegationRwdDetail>>>
where
    SA: LedgerAccess,
{
    // Convert from base64 representation
    let key: XfrPublicKey = wallet::public_key_from_base64(&info.address)
        .c(d!())
        .map_err(|e| error::ErrorBadRequest(e.generate_log()))?;

    let read = data.read();
    let staking = read.get_staking();

    let di = staking
        .delegation_get(&key)
        .c(d!())
        .map_err(error::ErrorBadRequest)?;

    Ok(web::Json(
        (0..=info.height)
            .into_iter()
            .rev()
            .filter_map(|i| di.rwd_detail.get(&i))
            .take(1)
            .cloned()
            .collect(),
    ))
}

#[derive(Deserialize, Debug)]
struct ValidatorDelegationQueryParams {
    address: TendermintAddr,
    epoch_size: u32,
    epoch_cnt: u8,
}

#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
struct ValidatorDelegation {
    return_rate: [u128; 2],
    self_delegation: u64,
    delegated: u64,
}

async fn get_validator_delegation_history<SA>(
    data: web::Data<Arc<RwLock<SA>>>,
    web::Query(info): web::Query<ValidatorDelegationQueryParams>,
) -> actix_web::Result<web::Json<Vec<ValidatorDelegation>>>
where
    SA: LedgerAccess,
{
    let read = data.read();
    let staking = read.get_staking();

    let v_id = staking
        .validator_td_addr_to_app_pk(info.address.as_ref())
        .c(d!())
        .map_err(error::ErrorBadRequest)?;
    let v_self_delegation = staking
        .delegation_get(&v_id)
        .ok_or_else(|| error::ErrorBadRequest("not exists"))?;

    let mut history = vec![];

    let self_delegation = v_self_delegation
        .entries
        .iter()
        .filter(|(k, _)| **k == v_id)
        .map(|(_, n)| n)
        .sum();
    let delegated = v_self_delegation.delegators.values().sum::<u64>();

    history.push(ValidatorDelegation {
        return_rate: Staking::get_block_rewards_rate(&*read),
        delegated,
        self_delegation,
    });

    let h = staking.cur_height();
    let epoch_size = info.epoch_size as u64;
    (1..=info.epoch_cnt as u64)
        .into_iter()
        .filter_map(|i| {
            if h >= i * epoch_size
                && h - i * epoch_size >= v_self_delegation.start_height
            {
                Some(h - i * epoch_size)
            } else {
                None
            }
        })
        .for_each(|h| {
            history.push(ValidatorDelegation {
                return_rate: *staking
                    .query_block_rewards_rate(&h)
                    .unwrap_or(&history.last().unwrap().return_rate), //unwrap is safe here
                delegated: {
                    if v_self_delegation.delegation_amount.is_empty()
                        || v_self_delegation
                            .delegation_amount
                            .iter()
                            .take(1)
                            .all(|(&i, _)| i > h)
                    {
                        0
                    } else {
                        *v_self_delegation
                            .delegation_amount
                            .get(&h)
                            .unwrap_or(&history.last().unwrap().delegated)
                    }
                },
                self_delegation: *v_self_delegation
                    .self_delegation_detail
                    .get(&h)
                    .unwrap_or(&history.last().unwrap().self_delegation),
            })
        });

    Ok(web::Json(history))
}

#[derive(Deserialize, Debug)]
struct DelegatorQueryParams {
    address: String,
    page: usize,
    per_page: usize,
    order: OrderOption,
}

#[derive(Deserialize, Debug, PartialEq)]
#[serde(rename_all = "snake_case")]
enum OrderOption {
    Desc,
    Asc,
}

async fn get_delegators_with_params<SA>(
    data: web::Data<Arc<RwLock<SA>>>,
    web::Query(info): web::Query<DelegatorQueryParams>,
) -> actix_web::Result<web::Json<DelegatorList>>
where
    SA: LedgerAccess,
{
    let read = data.read();
    let staking = read.get_staking();

    if info.page == 0 || info.order == OrderOption::Asc {
        return Ok(web::Json(DelegatorList::new(vec![])));
    }

    let start = (info.page - 1)
        .checked_mul(info.per_page)
        .c(d!())
        .map_err(error::ErrorBadRequest)?;
    let end = start
        .checked_add(info.per_page)
        .c(d!())
        .map_err(error::ErrorBadRequest)?;

    let list = staking
        .validator_get_delegator_list(info.address.as_ref(), start, end)
        .c(d!())
        .map_err(error::ErrorNotFound)?;

    let list: Vec<DelegatorInfo> = list
        .iter()
        .map(|(key, am)| DelegatorInfo::new(wallet::public_key_to_base64(key), **am))
        .collect();

    Ok(web::Json(DelegatorList::new(list)))
}

async fn query_delegator_list<SA>(
    data: web::Data<Arc<RwLock<SA>>>,
    addr: web::Path<TendermintAddr>,
) -> actix_web::Result<web::Json<DelegatorList>>
where
    SA: LedgerAccess,
{
    let read = data.read();
    let staking = read.get_staking();

    let list = staking
        .validator_get_delegator_list(addr.as_ref(), 0, usize::MAX)
        .c(d!())
        .map_err(error::ErrorNotFound)?;

    let list: Vec<DelegatorInfo> = list
        .iter()
        .map(|(key, am)| DelegatorInfo::new(wallet::public_key_to_base64(key), **am))
        .collect();

    Ok(web::Json(DelegatorList::new(list)))
}

async fn query_validator_detail<SA>(
    data: web::Data<Arc<RwLock<SA>>>,
    addr: web::Path<TendermintAddr>,
) -> actix_web::Result<web::Json<ValidatorDetail>>
where
    SA: LedgerAccess,
{
    let read = data.read();
    let staking = read.get_staking();

    let v_id = staking
        .validator_td_addr_to_app_pk(addr.as_ref())
        .c(d!())
        .map_err(error::ErrorBadRequest)?;
    let v_self_delegation = staking
        .delegation_get(&v_id)
        .ok_or_else(|| error::ErrorBadRequest("not exists"))?;

    if let Some(vd) = staking.validator_get_current() {
        if let Some(v) = vd.body.get(&v_id) {
            if 0 < v.td_power {
                let mut power_list =
                    vd.body.values().map(|v| v.td_power).collect::<Vec<_>>();
                power_list.sort_unstable();
                let voting_power_rank =
                    power_list.len() - power_list.binary_search(&v.td_power).unwrap();
                let realtime_rate = Staking::get_block_rewards_rate(&*read);
                let expected_annualization = [
                    realtime_rate[0] as u128
                        * v_self_delegation.proposer_rwd_cnt as u128,
                    realtime_rate[1] as u128
                        * (1 + staking.cur_height() - v_self_delegation.start_height)
                            as u128,
                ];

                let resp = ValidatorDetail {
                    addr: addr.into_inner(),
                    is_online: v.signed_last_block,
                    voting_power: v.td_power,
                    voting_power_rank,
                    commission_rate: v.get_commission_rate(),
                    self_staking: v_self_delegation
                        .entries
                        .iter()
                        .filter(|(k, _)| **k == v_id)
                        .map(|(_, n)| n)
                        .sum(),
                    fra_rewards: v_self_delegation.rwd_amount,
                    memo: v.memo.clone(),
                    start_height: v_self_delegation.start_height,
                    cur_height: staking.cur_height(),
                    block_signed_cnt: v.signed_cnt,
                    block_proposed_cnt: v_self_delegation.proposer_rwd_cnt,
                    expected_annualization,
                    kind: v.kind(),
                };
                return Ok(web::Json(resp));
            }
        }
    }

    Err(error::ErrorNotFound("not exists"))
}

async fn query_delegation_info<SA>(
    data: web::Data<Arc<RwLock<SA>>>,
    address: web::Path<String>,
) -> actix_web::Result<web::Json<DelegationInfo>>
where
    SA: LedgerAccess,
{
    let pk = wallet::public_key_from_base64(address.as_str())
        .c(d!())
        .map_err(|e| error::ErrorBadRequest(e.generate_log()))?;

    let read = data.read();
    let staking = read.get_staking();

    let block_rewards_rate = Staking::get_block_rewards_rate(&*read);
    let global_staking = staking.validator_global_power();
    let global_delegation = staking.delegation_info_global_amount();

    let (
        bond_amount,
        bond_entries,
        unbond_amount,
        rwd_amount,
        start_height,
        end_height,
        delegation_rwd_cnt,
        proposer_rwd_cnt,
    ) = staking
        .delegation_get(&pk)
        .map(|d| {
            let mut bond_amount = d.amount();
            let bond_entries: Vec<(String, u64)> = d
                .entries
                .iter()
                .filter_map(|(pk, am)| {
                    staking
                        .validator_app_pk_to_td_addr(pk)
                        .ok()
                        .map(|addr| (addr, *am))
                })
                .collect();
            let mut unbond_amount = 0;
            match d.state {
                DelegationState::Paid => {
                    bond_amount = 0;
                }
                DelegationState::Free => {
                    mem::swap(&mut bond_amount, &mut unbond_amount);
                }
                DelegationState::Bond => {
                    if staking.cur_height()
                        > d.end_height().saturating_sub(UNBOND_BLOCK_CNT)
                    {
                        mem::swap(&mut bond_amount, &mut unbond_amount);
                    }
                }
            }
            (
                bond_amount,
                bond_entries,
                unbond_amount,
                d.rwd_amount,
                d.start_height(),
                d.end_height(),
                d.delegation_rwd_cnt,
                d.proposer_rwd_cnt,
            )
        })
        .unwrap_or((0, vec![], 0, 0, 0, 0, 0, 0));

    let mut resp = DelegationInfo::new(
        bond_amount,
        bond_entries,
        unbond_amount,
        rwd_amount,
        block_rewards_rate,
        global_delegation,
        global_staking,
    );
    resp.start_height = start_height;
    resp.current_height = staking.cur_height();
    resp.end_height = end_height;
    resp.delegation_rwd_cnt = delegation_rwd_cnt;
    resp.proposer_rwd_cnt = proposer_rwd_cnt;

    Ok(web::Json(resp))
}

async fn query_owned_utxos<LA>(
    data: web::Data<Arc<RwLock<LA>>>,
    owner: web::Path<String>,
) -> actix_web::Result<web::Json<BTreeMap<TxoSID, (Utxo, Option<OwnerMemo>)>>>
where
    LA: LedgerAccess,
{
    wallet::public_key_from_base64(owner.as_str())
        .c(d!())
        .map_err(|e| error::ErrorBadRequest(e.generate_log()))
        .map(|pk| web::Json(data.read().get_owned_utxos(&pk)))
}

enum AccessApi {
    Ledger,
    Archive,
    Staking,
}

pub enum StakingAccessRoutes {
    ValidatorList,
    DelegationInfo,
    DelegatorList,
    ValidatorDetail,
}

impl NetworkRoute for StakingAccessRoutes {
    fn route(&self) -> String {
        let endpoint = match *self {
            StakingAccessRoutes::ValidatorList => "validator_list",
            StakingAccessRoutes::DelegationInfo => "delegation_info",
            StakingAccessRoutes::DelegatorList => "delegator_list",
            StakingAccessRoutes::ValidatorDetail => "validator_detail",
        };
        "/".to_owned() + endpoint
    }
}

pub enum LedgerAccessRoutes {
    UtxoSid,
    UtxoSidList,
    AssetIssuanceNum,
    AssetToken,
    PublicKey,
    GlobalState,
}

impl NetworkRoute for LedgerAccessRoutes {
    fn route(&self) -> String {
        let endpoint = match *self {
            LedgerAccessRoutes::UtxoSid => "utxo_sid",
            LedgerAccessRoutes::UtxoSidList => "utxo_sid_list",
            LedgerAccessRoutes::AssetIssuanceNum => "asset_issuance_num",
            LedgerAccessRoutes::AssetToken => "asset_token",
            LedgerAccessRoutes::PublicKey => "public_key",
            LedgerAccessRoutes::GlobalState => "global_state",
        };
        "/".to_owned() + endpoint
    }
}

pub enum LedgerArchiveRoutes {
    TxnSid,
    AirAddress,
    BlockLog,
    GlobalStateVersion,
    BlocksSince,
    UtxoMap,
    UtxoMapChecksum,
    UtxoPartialMap,
    OwnedUtxos,
}

impl NetworkRoute for LedgerArchiveRoutes {
    fn route(&self) -> String {
        let endpoint = match *self {
            LedgerArchiveRoutes::TxnSid => "txn_sid",
            LedgerArchiveRoutes::AirAddress => "air_address",
            LedgerArchiveRoutes::BlockLog => "block_log",
            LedgerArchiveRoutes::BlocksSince => "blocks_since",
            LedgerArchiveRoutes::GlobalStateVersion => "global_state_version",
            LedgerArchiveRoutes::UtxoMap => "utxo_map",
            LedgerArchiveRoutes::UtxoMapChecksum => "utxo_map_checksum",
            LedgerArchiveRoutes::UtxoPartialMap => "utxo_partial_map",
            LedgerArchiveRoutes::OwnedUtxos => "owned_utxos",
        };
        "/".to_owned() + endpoint
    }
}

trait Route {
    fn set_route<LA: 'static + LedgerAccess + LedgerAccess + Sync + Send>(
        self,
        service_interface: AccessApi,
    ) -> Self;

    fn set_route_for_ledger_access<LA: 'static + LedgerAccess + Sync + Send>(
        self,
    ) -> Self;

    fn set_route_for_archive_access<LA: 'static + LedgerAccess + Sync + Send>(
        self,
    ) -> Self;

    fn set_route_for_staking_access<LA: 'static + LedgerAccess + Sync + Send>(
        self,
    ) -> Self;
}

impl<T, B> Route for App<T, B>
where
    B: actix_web::dev::MessageBody,
    T: actix_service::ServiceFactory<
        Config = (),
        Request = dev::ServiceRequest,
        Response = dev::ServiceResponse<B>,
        Error = error::Error,
        InitError = (),
    >,
{
    // Call the appropraite function depending on the interface
    fn set_route<LA: 'static + LedgerAccess + Sync + Send>(
        self,
        service_interface: AccessApi,
    ) -> Self {
        match service_interface {
            AccessApi::Ledger => self.set_route_for_ledger_access::<LA>(),
            AccessApi::Archive => self.set_route_for_archive_access::<LA>(),
            AccessApi::Staking => self.set_route_for_staking_access::<LA>(),
        }
    }

    // Set routes for the LedgerAccess interface
    fn set_route_for_ledger_access<LA: 'static + LedgerAccess + Sync + Send>(
        self,
    ) -> Self {
        self.route(
            &LedgerAccessRoutes::UtxoSid.with_arg_template("sid"),
            web::get().to(query_utxo::<LA>),
        )
        .route(
            &LedgerAccessRoutes::UtxoSidList.with_arg_template("sid_list"),
            web::get().to(query_utxos::<LA>),
        )
        .route(
            &LedgerAccessRoutes::AssetIssuanceNum.with_arg_template("code"),
            web::get().to(query_asset_issuance_num::<LA>),
        )
        .route(
            &LedgerAccessRoutes::AssetToken.with_arg_template("code"),
            web::get().to(query_asset::<LA>),
        )
        .route(
            &LedgerAccessRoutes::PublicKey.route(),
            web::get().to(query_public_key::<LA>),
        )
        .route(
            &LedgerAccessRoutes::GlobalState.route(),
            web::get().to(query_global_state::<LA>),
        )
    }

    // Set routes for the LedgerAccess interface
    fn set_route_for_archive_access<LA: 'static + LedgerAccess + Sync + Send>(
        self,
    ) -> Self {
        self.route(
            &LedgerArchiveRoutes::TxnSid.with_arg_template("sid"),
            web::get().to(query_txn::<LA>),
        )
        .route(
            &LedgerArchiveRoutes::BlockLog.route(),
            web::get().to(query_block_log::<LA>),
        )
        .route(
            &LedgerArchiveRoutes::GlobalStateVersion.with_arg_template("version"),
            web::get().to(query_global_state_version::<LA>),
        )
        .route(
            &LedgerArchiveRoutes::BlocksSince.with_arg_template("block_sid"),
            web::get().to(query_blocks_since::<LA>),
        )
        .route(
            &LedgerArchiveRoutes::UtxoMap.route(),
            web::get().to(query_utxo_map::<LA>),
        )
        .route(
            &LedgerArchiveRoutes::UtxoMapChecksum.route(),
            web::get().to(query_utxo_map_checksum::<LA>),
        )
        .route(
            &LedgerArchiveRoutes::OwnedUtxos.with_arg_template("owner"),
            web::get().to(query_owned_utxos::<LA>),
        )
    }

    fn set_route_for_staking_access<SA: 'static + LedgerAccess + Sync + Send>(
        self,
    ) -> Self {
        self.route(
            &StakingAccessRoutes::ValidatorList.route(),
            web::get().to(query_validators::<SA>),
        )
        .route(
            &StakingAccessRoutes::DelegationInfo.with_arg_template("XfrPublicKey"),
            web::get().to(query_delegation_info::<SA>),
        )
        .route(
            &StakingAccessRoutes::DelegatorList.with_arg_template("NodeAddress"),
            web::get().to(query_delegator_list::<SA>),
        )
        .service(
            web::resource("/delegator_list")
                .route(web::get().to(get_delegators_with_params::<SA>)),
        )
        .service(
            web::resource("/delegation_rewards")
                .route(web::get().to(get_delegation_reward::<SA>)),
        )
        .service(
            web::resource("/validator_delegation")
                .route(web::get().to(get_validator_delegation_history::<SA>)),
        )
        .route(
            &StakingAccessRoutes::ValidatorDetail.with_arg_template("NodeAddress"),
            web::get().to(query_validator_detail::<SA>),
        )
    }
}

impl RestfulApiService {
    pub fn create<LA: 'static + LedgerAccess + Sync + Send>(
        ledger_access: Arc<RwLock<LA>>,
        host: &str,
        port: u16,
    ) -> Result<RestfulApiService> {
        let web_runtime = actix_rt::System::new("findora API");

        HttpServer::new(move || {
            App::new()
                .wrap(middleware::Logger::default())
                .wrap(Cors::permissive().supports_credentials())
                .data(ledger_access.clone())
                .route("/ping", web::get().to(ping))
                .route("/version", web::get().to(version))
                .set_route::<LA>(AccessApi::Ledger)
                .set_route::<LA>(AccessApi::Archive)
                .set_route::<LA>(AccessApi::Staking)
        })
        .bind(&format!("{}:{}", host, port))
        .c(d!())?
        .run();

        info!("RestfulApi server started");

        Ok(RestfulApiService { web_runtime })
    }
    // call from a thread; this will block.
    pub fn run(self) -> Result<()> {
        self.web_runtime.run().c(d!())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use actix_service::Service;
    use actix_web::{test, web, App};
    use futures::executor;
    use ledger::data_model::{Operation, Transaction, TxnEffect};
    use ledger::store::{helpers::*, LedgerState, LedgerUpdate};
    use rand_chacha::ChaChaRng;
    use rand_core::SeedableRng;

    #[test]
    fn test_query_utxo() {}

    #[test]
    fn test_query_txn() {}

    #[test]
    fn test_query_policy() {}

    #[test]
    fn test_query_proof() {}

    #[test]
    fn test_query_contract() {}

    #[test]
    #[allow(clippy::type_complexity)]
    fn test_query_state_commitment() {
        let mut prng = ChaChaRng::from_seed([0u8; 32]);
        let mut state = LedgerState::test_ledger();
        let (_, seq_id) = state.get_state_commitment();
        let mut tx = Transaction::from_seq_id(seq_id);

        let token_code1 = AssetTypeCode::gen_random();
        let keypair = build_keys(&mut prng);

        let asset_body = asset_creation_body(
            &token_code1,
            keypair.get_pk_ref(),
            AssetRules::default(),
            None,
            None,
        );
        let asset_create = asset_creation_operation(&asset_body, &keypair);
        tx.body
            .operations
            .push(Operation::DefineAsset(asset_create));

        let effect = TxnEffect::compute_effect(tx).unwrap();
        {
            let mut block = state.start_block().unwrap();
            state.apply_transaction(&mut block, effect, false).unwrap();
            state.finish_block(block).unwrap();
        }

        let state_lock = Arc::new(RwLock::new(state));

        let mut app = executor::block_on(test::init_service(
            App::new()
                .data(state_lock.clone())
                .route(
                    "/global_state",
                    web::get().to(query_global_state::<LedgerState>),
                )
                .route(
                    "/global_state_version/{version}",
                    web::get().to(query_global_state_version::<LedgerState>),
                ),
        ));

        let req = test::TestRequest::get().uri("/global_state").to_request();

        let second_req = test::TestRequest::get()
            .uri("/global_state_version/1")
            .to_request();

        let state_reader = state_lock.read();
        let (comm1, idx, _sig): (
            _,
            _,
            SignatureOf<(HashOf<Option<StateCommitmentData>>, u64)>,
        ) = executor::block_on(test::read_response_json(&mut app, req));
        let comm2 = executor::block_on(test::read_response_json(&mut app, second_req));
        assert!((comm1, idx) == state_reader.get_state_commitment());
        assert!((comm2, idx) == state_reader.get_state_commitment());
    }

    #[test]
    // Tests that the server
    //  (a) responds with the same public key across a transaction
    //  (b) responds to /global_state with a response signed by the public
    //      key from /public_key
    #[allow(clippy::type_complexity)]
    fn test_query_public_key() {
        let mut prng = ChaChaRng::from_seed([0u8; 32]);
        let mut state = LedgerState::test_ledger();
        let (_, seq_id) = state.get_state_commitment();
        let mut tx = Transaction::from_seq_id(seq_id);

        let orig_key = *state.public_key();

        let token_code1 = AssetTypeCode::gen_random();
        let keypair = build_keys(&mut prng);

        let asset_body = asset_creation_body(
            &token_code1,
            keypair.get_pk_ref(),
            AssetRules::default(),
            None,
            None,
        );
        let asset_create = asset_creation_operation(&asset_body, &keypair);
        tx.body
            .operations
            .push(Operation::DefineAsset(asset_create));

        let effect = TxnEffect::compute_effect(tx).unwrap();
        {
            let mut block = state.start_block().unwrap();
            state.apply_transaction(&mut block, effect, false).unwrap();
            state.finish_block(block).unwrap();
        }

        let state_lock = Arc::new(RwLock::new(state));

        let mut app = executor::block_on(test::init_service(
            App::new()
                .data(state_lock.clone())
                .route(
                    "/global_state",
                    web::get().to(query_global_state::<LedgerState>),
                )
                .route(
                    "/public_key",
                    web::get().to(query_public_key::<LedgerState>),
                ),
        ));

        let req_pk = test::TestRequest::get().uri("/public_key").to_request();
        let req_comm = test::TestRequest::get().uri("/global_state").to_request();

        let state_reader = state_lock.read();
        let k: XfrPublicKey =
            executor::block_on(test::read_response_json(&mut app, req_pk));
        let (comm, idx, sig): (
            HashOf<Option<StateCommitmentData>>,
            u64,
            SignatureOf<(HashOf<Option<StateCommitmentData>>, u64)>,
        ) = executor::block_on(test::read_response_json(&mut app, req_comm));
        sig.verify(&k, &(comm, idx)).unwrap();
        assert!(k == orig_key);
        assert!(k == *state_reader.public_key());
    }

    #[test]
    fn test_query_asset() {
        let mut prng = ChaChaRng::from_entropy();
        let mut state = LedgerState::test_ledger();
        let (_, seq_id) = state.get_state_commitment();
        let mut tx = Transaction::from_seq_id(seq_id);

        let token_code1 = AssetTypeCode::gen_random();
        let keypair = build_keys(&mut prng);

        let asset_body = asset_creation_body(
            &token_code1,
            keypair.get_pk_ref(),
            AssetRules::default(),
            None,
            None,
        );
        let asset_create = asset_creation_operation(&asset_body, &keypair);
        tx.body
            .operations
            .push(Operation::DefineAsset(asset_create));

        let effect = TxnEffect::compute_effect(tx).unwrap();
        {
            let mut block = state.start_block().unwrap();
            state.apply_transaction(&mut block, effect, false).unwrap();
            state.finish_block(block).unwrap();
        }

        let mut app = executor::block_on(test::init_service(
            App::new().data(Arc::new(RwLock::new(state))).route(
                "/asset_token/{token}",
                web::get().to(query_asset::<LedgerState>),
            ),
        ));

        let req = test::TestRequest::get()
            .uri(&format!("/asset_token/{}", token_code1.to_base64()))
            .to_request();
        let resp = executor::block_on(app.call(req)).unwrap();

        assert!(resp.status().is_success());
    }
}
