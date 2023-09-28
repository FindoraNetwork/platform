//!
//! # Access Ledger Data
//!

use {
    super::server::QueryServer,
    actix_web::{error, web},
    config::abci::global_cfg::CFG,
    finutils::api::{DelegationInfo, NetworkRoute, ValidatorDetail},
    globutils::HashOf,
    ledger::{
        data_model::{
            AssetType, AssetTypeCode, AuthenticatedUtxo, StateCommitmentData, TxnSID,
            TxoSID, UnAuthenticatedUtxo, Utxo,
        },
        staking::{DelegationState, TendermintAddr},
    },
    parking_lot::RwLock,
    ruc::*,
    serde::Deserialize,
    std::{collections::BTreeMap, mem, sync::Arc},
    zei::xfr::structs::OwnerMemo,
};

/// Ping route to check for liveness of API
#[allow(clippy::unnecessary_wraps)]
pub async fn ping() -> actix_web::Result<String> {
    Ok("success".into())
}

/// query utxo according to `TxoSID` return Authenticated Utxo
pub async fn query_utxo(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<AuthenticatedUtxo>> {
    let qs = data.read();
    let ledger = &qs.ledger_cloned;
    if let Ok(txo_sid) = info.parse::<u64>() {
        if let Some(txo) = ledger.get_utxo(TxoSID(txo_sid)) {
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

/// query utxo according to `TxoSID` return UnAuthenticated Utxo
pub async fn query_utxo_light(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<UnAuthenticatedUtxo>> {
    let qs = data.read();
    let ledger = &qs.ledger_cloned;
    if let Ok(txo_sid) = info.parse::<u64>() {
        if let Some(txo) = ledger.get_utxo_light(TxoSID(txo_sid)) {
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

/// query issuance num according to `AssetTypeCode`
pub async fn query_asset_issuance_num(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<u64>> {
    let qs = data.read();
    let ledger = &qs.ledger_cloned;
    if let Ok(token_code) = AssetTypeCode::new_from_base64(&info) {
        if let Some(iss_num) = ledger.get_issuance_num(&token_code) {
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

/// Separate a string of `TxoSID` by ',' and query the corresponding Authenticated utxo
pub async fn query_utxos(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<Vec<Option<AuthenticatedUtxo>>>> {
    let sid_list = info
        .as_ref()
        .split(',')
        .map(|i| {
            i.parse::<u64>()
                .map(TxoSID)
                .map_err(actix_web::error::ErrorBadRequest)
        })
        .collect::<actix_web::Result<Vec<_>, actix_web::error::Error>>()?;

    let qs = data.read();
    let ledger = &qs.ledger_cloned;

    if sid_list.len() > 10 || sid_list.is_empty() {
        return Err(actix_web::error::ErrorBadRequest("Invalid Query List"));
    }
    match ledger.get_utxos(sid_list.as_slice()) {
        Ok(v) => Ok(web::Json(v)),
        Err(e) => Err(actix_web::error::ErrorBadRequest(format!("{:?}", e))),
    }
}

/// query asset according to `AssetType`
pub async fn query_asset(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<AssetType>> {
    let qs = data.read();
    let ledger = &qs.ledger_cloned;
    if let Ok(token_code) = AssetTypeCode::new_from_base64(&info) {
        if let Some(asset) = ledger.get_asset_type(&token_code) {
            Ok(web::Json(asset))
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

/// query tx according to `TxnSID`
pub async fn query_txn(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<String>,
) -> actix_web::Result<String> {
    let qs = data.read();
    let ledger = &qs.ledger_cloned;
    if let Ok(txn_sid) = info.parse::<usize>() {
        if let Ok(mut txn) = ruc::info!(ledger.get_transaction(TxnSID(txn_sid))) {
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

/// query tx according to `TxnSID`, lighter and faster version
pub async fn query_txn_light(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    info: web::Path<String>,
) -> actix_web::Result<String> {
    let qs = data.read();
    let ledger = &qs.ledger_cloned;
    if let Ok(txn_sid) = info.parse::<usize>() {
        if let Ok(mut txn) = ruc::info!(ledger.get_transaction_light(TxnSID(txn_sid))) {
            txn.set_txo_id();
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

/// query global state, return (apphash, block count, apphash and block count signatures)
#[allow(clippy::type_complexity)]
pub async fn query_global_state(
    data: web::Data<Arc<RwLock<QueryServer>>>,
) -> web::Json<(HashOf<Option<StateCommitmentData>>, u64, &'static str)> {
    let qs = data.read();
    let ledger = &qs.ledger_cloned;
    let (hash, seq_id) = ledger.get_state_commitment();

    web::Json((hash, seq_id, "v4UVgkIBpj0eNYI1B1QhTTduJHCIHH126HcdesCxRdLkVGDKrVUPgwmNLCDafTVgC5e4oDhAGjPNt1VhUr6ZCQ=="))
}

/// query global state version according to `block_height`
pub async fn query_global_state_version(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    version: web::Path<u64>,
) -> web::Json<Option<HashOf<Option<StateCommitmentData>>>> {
    let qs = data.read();
    let ledger = &qs.ledger_cloned;
    let hash = ledger.get_state_commitment_at_block_height(*version);
    web::Json(hash)
}

#[derive(Deserialize, Debug, PartialEq)]
#[serde(rename_all = "snake_case")]
enum OrderOption {
    Desc,
    Asc,
}

/// query validator detail according to `TendermintAddr`
pub async fn query_validator_detail(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    addr: web::Path<TendermintAddr>,
) -> actix_web::Result<web::Json<ValidatorDetail>> {
    let qs = data.read();
    let ledger = &qs.ledger_cloned;
    let staking = ledger.get_staking();

    // Get Pub key from from the provided tendermint address
    let v_pub_key = info!(staking.validator_td_addr_to_app_pk(addr.as_ref()))
        .map_err(error::ErrorBadRequest)?;
    // Get Self delegation for the validator
    // v_self_delegation is the delegation details for the validator
    let v_self_delegation =
        info!(staking.delegation_get(&v_pub_key)).map_err(error::ErrorBadRequest)?;

    if let Some(vd) = staking.validator_get_current() {
        if let Some(v) = vd.body.get(&v_pub_key) {
            let voting_power_rank = if 0 == v.td_power {
                100_0000 + vd.body.len()
            } else {
                let mut power_list =
                    vd.body.values().map(|v| v.td_power).collect::<Vec<_>>();
                power_list.sort_unstable();
                power_list.len() - power_list.binary_search(&v.td_power).unwrap()
            };
            // Network Realtime APY
            let network_realtime_apy = ledger.staking_get_block_rewards_rate();

            let commission_rate = v.get_commission_rate();

            // Latest expected Return = % APY (annual) * (1 - commission %)
            let one_sub_commission_rate = [
                commission_rate[1] as u128 - commission_rate[0] as u128,
                commission_rate[1] as u128,
            ];
            let validator_realtime_apy = [
                network_realtime_apy[0] * one_sub_commission_rate[0],
                network_realtime_apy[1] * one_sub_commission_rate[1],
            ];

            // fra_rewards: all delegators rewards including self-delegation
            let mut fra_rewards = v_self_delegation.rwd_amount;
            for (delegator, _) in &v.delegators {
                let delegation = staking
                    .delegation_get(&delegator)
                    .ok_or_else(|| error::ErrorBadRequest("not exists"))?;
                fra_rewards += delegation.rwd_amount;
            }
            let resp = ValidatorDetail {
                addr: addr.into_inner(),
                is_online: v.signed_last_block,
                voting_power: v.td_power,
                voting_power_rank,
                commission_rate,
                self_staking: v_self_delegation
                    .delegations
                    .iter()
                    .filter(|(k, _)| **k == v_pub_key)
                    .map(|(_, n)| n)
                    .sum(),
                fra_rewards,
                memo: v.memo.clone(),
                start_height: v_self_delegation.start_height,
                cur_height: staking.cur_height(),
                block_signed_cnt: v.signed_cnt,
                block_proposed_cnt: v_self_delegation.proposer_rwd_cnt,
                validator_realtime_apy,
                kind: v.kind(),
                delegator_cnt: v.delegators.len() as u64,
            };
            return Ok(web::Json(resp));
        }
    }

    Err(error::ErrorNotFound("not exists"))
}

/// query delegation info according to `public_key`
pub async fn query_delegation_info(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    address: web::Path<String>,
) -> actix_web::Result<web::Json<DelegationInfo>> {
    let pk = globutils::wallet::public_key_from_base64(address.as_str())
        .c(d!())
        .map_err(|e| error::ErrorBadRequest(e.to_string()))?;

    let qs = data.read();
    let ledger = &qs.ledger_cloned;
    let staking = ledger.get_staking();

    let block_rewards_rate = ledger.staking_get_block_rewards_rate();
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
                .delegations
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
                        > d.end_height()
                            .saturating_sub(CFG.checkpoint.unbond_block_cnt)
                    {
                        mem::swap(&mut bond_amount, &mut unbond_amount);
                    }
                }
            }
            // check temporary partial undelegators
            unbond_amount += d.tmp_delegators.values().sum::<u64>();

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

/// query utxos according `public_key`
pub async fn query_owned_utxos(
    data: web::Data<Arc<RwLock<QueryServer>>>,
    owner: web::Path<String>,
) -> actix_web::Result<web::Json<BTreeMap<TxoSID, (Utxo, Option<OwnerMemo>)>>> {
    let qs = data.read();
    let ledger = &qs.ledger_cloned;
    globutils::wallet::public_key_from_base64(owner.as_str())
        .c(d!())
        .map_err(|e| error::ErrorBadRequest(e.to_string()))
        .map(|pk| web::Json(pnk!(ledger.get_owned_utxos(&pk))))
}

#[allow(missing_docs)]
pub enum ApiRoutes {
    UtxoSid,
    UtxoSidLight,
    UtxoSidList,
    AssetIssuanceNum,
    AssetToken,
    GlobalState,
    TxnSid,
    TxnSidLight,
    GlobalStateVersion,
    OwnedUtxos,
    ValidatorList,
    DelegationInfo,
    DelegatorList,
    ValidatorDetail,
}

impl NetworkRoute for ApiRoutes {
    fn route(&self) -> String {
        let endpoint = match *self {
            ApiRoutes::UtxoSid => "utxo_sid",
            ApiRoutes::UtxoSidLight => "utxo_sid_light",
            ApiRoutes::UtxoSidList => "utxo_sid_list",
            ApiRoutes::AssetIssuanceNum => "asset_issuance_num",
            ApiRoutes::AssetToken => "asset_token",
            ApiRoutes::GlobalState => "global_state",
            ApiRoutes::TxnSid => "txn_sid",
            ApiRoutes::TxnSidLight => "txn_sid_light",
            ApiRoutes::GlobalStateVersion => "global_state_version",
            ApiRoutes::OwnedUtxos => "owned_utxos",
            ApiRoutes::ValidatorList => "validator_list",
            ApiRoutes::DelegationInfo => "delegation_info",
            ApiRoutes::DelegatorList => "delegator_list",
            ApiRoutes::ValidatorDetail => "validator_detail",
        };
        "/".to_owned() + endpoint
    }
}
