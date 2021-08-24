#[cfg(test)]
#[allow(missing_docs)]
mod test;

use actix_cors::Cors;
use actix_web::{dev, error, middleware, web, App, HttpServer};
use finutils::api::{
    DelegationInfo, DelegatorInfo, DelegatorList, NetworkRoute, Validator,
    ValidatorDetail, ValidatorList,
};
use globutils::{HashOf, SignatureOf};
use ledger::{
    data_model::{
        AssetType, AssetTypeCode, AuthenticatedUtxo, StateCommitmentData, TxnSID,
        TxoSID, UnAuthenticatedUtxo, Utxo,
    },
    staking::{
        DelegationRwdDetail, DelegationState, Staking, TendermintAddr, UNBOND_BLOCK_CNT,
    },
    store::LedgerState,
};
use log::info;
use parking_lot::RwLock;
use ruc::*;
use serde::{Deserialize, Serialize};
use std::{collections::BTreeMap, mem, sync::Arc};
use zei::xfr::{sig::XfrPublicKey, structs::OwnerMemo};

pub struct RestfulApiService;

// Ping route to check for liveness of API
#[allow(clippy::unnecessary_wraps)]
async fn ping() -> actix_web::Result<String> {
    Ok("success".into())
}

// Returns the git commit hash and commit date of this build
#[allow(clippy::unnecessary_wraps)]
async fn version() -> actix_web::Result<String> {
    Ok(format!(
        "Build: {} {}",
        option_env!("VERGEN_SHA_EXTERN").unwrap_or(env!("VERGEN_SHA")),
        env!("VERGEN_BUILD_DATE")
    ))
}

pub async fn query_utxo(
    data: web::Data<Arc<RwLock<LedgerState>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<AuthenticatedUtxo>> {
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

pub async fn query_utxo_light(
    data: web::Data<Arc<RwLock<LedgerState>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<UnAuthenticatedUtxo>> {
    let reader = data.read();
    if let Ok(txo_sid) = info.parse::<u64>() {
        if let Some(txo) = reader.get_utxo_light(TxoSID(txo_sid)) {
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

pub async fn query_asset_issuance_num(
    data: web::Data<Arc<RwLock<LedgerState>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<u64>> {
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

pub async fn query_utxos(
    data: web::Data<Arc<RwLock<LedgerState>>>,
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

    let reader = data.read();

    if sid_list.len() > 10 || sid_list.is_empty() {
        return Err(actix_web::error::ErrorBadRequest("Invalid Query List"));
    }

    Ok(web::Json(reader.get_utxos(sid_list.as_slice())))
}

pub async fn query_asset(
    data: web::Data<Arc<RwLock<LedgerState>>>,
    info: web::Path<String>,
) -> actix_web::Result<web::Json<AssetType>> {
    let reader = data.read();
    if let Ok(token_code) = AssetTypeCode::new_from_base64(&*info) {
        if let Some(asset) = reader.get_asset_type(&token_code) {
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

pub async fn query_txn(
    data: web::Data<Arc<RwLock<LedgerState>>>,
    info: web::Path<String>,
) -> actix_web::Result<String> {
    let reader = data.read();
    if let Ok(txn_sid) = info.parse::<usize>() {
        if let Ok(mut txn) = ruc::info!(reader.get_transaction(TxnSID(txn_sid))) {
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

pub async fn query_txn_light(
    data: web::Data<Arc<RwLock<LedgerState>>>,
    info: web::Path<String>,
) -> actix_web::Result<String> {
    let reader = data.read();
    if let Ok(txn_sid) = info.parse::<usize>() {
        if let Ok(mut txn) = ruc::info!(reader.get_transaction_light(TxnSID(txn_sid))) {
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

pub async fn query_public_key(
    data: web::Data<Arc<RwLock<LedgerState>>>,
) -> web::Json<XfrPublicKey> {
    let reader = data.read();
    web::Json(*reader.public_key())
}

#[allow(clippy::type_complexity)]
pub async fn query_global_state(
    data: web::Data<Arc<RwLock<LedgerState>>>,
) -> web::Json<(
    HashOf<Option<StateCommitmentData>>,
    u64,
    SignatureOf<(HashOf<Option<StateCommitmentData>>, u64)>,
)> {
    let reader = data.read();
    let (hash, seq_id) = reader.get_state_commitment();
    let sig = reader.sign_message(&(hash.clone(), seq_id));
    web::Json((hash, seq_id, sig))
}

pub async fn query_global_state_version(
    data: web::Data<Arc<RwLock<LedgerState>>>,
    version: web::Path<u64>,
) -> web::Json<Option<HashOf<Option<StateCommitmentData>>>> {
    let reader = data.read();
    let hash = reader.get_state_commitment_at_block_height(*version);
    web::Json(hash)
}

// Query current validator list,
// validtors who have not completed self-deletagion will be filtered out.
#[allow(unused)]
async fn query_validators(
    data: web::Data<Arc<RwLock<LedgerState>>>,
) -> actix_web::Result<web::Json<ValidatorList>> {
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

async fn get_delegation_reward(
    data: web::Data<Arc<RwLock<LedgerState>>>,
    web::Query(info): web::Query<DelegationRwdQueryParams>,
) -> actix_web::Result<web::Json<Vec<DelegationRwdDetail>>> {
    // Convert from base64 representation
    let key: XfrPublicKey = globutils::wallet::public_key_from_base64(&info.address)
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

async fn get_validator_delegation_history(
    data: web::Data<Arc<RwLock<LedgerState>>>,
    web::Query(info): web::Query<ValidatorDelegationQueryParams>,
) -> actix_web::Result<web::Json<Vec<ValidatorDelegation>>> {
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

async fn get_delegators_with_params(
    data: web::Data<Arc<RwLock<LedgerState>>>,
    web::Query(info): web::Query<DelegatorQueryParams>,
) -> actix_web::Result<web::Json<DelegatorList>> {
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
        .map(|(key, am)| {
            DelegatorInfo::new(globutils::wallet::public_key_to_base64(key), **am)
        })
        .collect();

    Ok(web::Json(DelegatorList::new(list)))
}

async fn query_delegator_list(
    data: web::Data<Arc<RwLock<LedgerState>>>,
    addr: web::Path<TendermintAddr>,
) -> actix_web::Result<web::Json<DelegatorList>> {
    let read = data.read();
    let staking = read.get_staking();

    let list = staking
        .validator_get_delegator_list(addr.as_ref(), 0, usize::MAX)
        .c(d!())
        .map_err(error::ErrorNotFound)?;

    let list: Vec<DelegatorInfo> = list
        .iter()
        .map(|(key, am)| {
            DelegatorInfo::new(globutils::wallet::public_key_to_base64(key), **am)
        })
        .collect();

    Ok(web::Json(DelegatorList::new(list)))
}

async fn query_validator_detail(
    data: web::Data<Arc<RwLock<LedgerState>>>,
    addr: web::Path<TendermintAddr>,
) -> actix_web::Result<web::Json<ValidatorDetail>> {
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

async fn query_delegation_info(
    data: web::Data<Arc<RwLock<LedgerState>>>,
    address: web::Path<String>,
) -> actix_web::Result<web::Json<DelegationInfo>> {
    let pk = globutils::wallet::public_key_from_base64(address.as_str())
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

async fn query_owned_utxos(
    data: web::Data<Arc<RwLock<LedgerState>>>,
    owner: web::Path<String>,
) -> actix_web::Result<web::Json<BTreeMap<TxoSID, (Utxo, Option<OwnerMemo>)>>> {
    globutils::wallet::public_key_from_base64(owner.as_str())
        .c(d!())
        .map_err(|e| error::ErrorBadRequest(e.generate_log()))
        .map(|pk| web::Json(pnk!(data.read().get_owned_utxos(&pk))))
}

pub enum ApiRoutes {
    UtxoSid,
    UtxoSidLight,
    UtxoSidList,
    AssetIssuanceNum,
    AssetToken,
    PublicKey,
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
            ApiRoutes::PublicKey => "public_key",
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

trait Route {
    fn set_route(self) -> Self;
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
    fn set_route(self) -> Self {
        self.route("/ping", web::get().to(ping))
            .route("/version", web::get().to(version))
            .route(
                &ApiRoutes::UtxoSid.with_arg_template("sid"),
                web::get().to(query_utxo),
            )
            .route(
                &ApiRoutes::UtxoSidLight.with_arg_template("sid"),
                web::get().to(query_utxo_light),
            )
            .route(
                &ApiRoutes::UtxoSidList.with_arg_template("sid_list"),
                web::get().to(query_utxos),
            )
            .route(
                &ApiRoutes::AssetIssuanceNum.with_arg_template("code"),
                web::get().to(query_asset_issuance_num),
            )
            .route(
                &ApiRoutes::AssetToken.with_arg_template("code"),
                web::get().to(query_asset),
            )
            .route(
                &ApiRoutes::PublicKey.route(),
                web::get().to(query_public_key),
            )
            .route(
                &ApiRoutes::GlobalState.route(),
                web::get().to(query_global_state),
            )
            .route(
                &ApiRoutes::TxnSid.with_arg_template("sid"),
                web::get().to(query_txn),
            )
            .route(
                &ApiRoutes::TxnSidLight.with_arg_template("sid"),
                web::get().to(query_txn_light),
            )
            .route(
                &ApiRoutes::GlobalStateVersion.with_arg_template("version"),
                web::get().to(query_global_state_version),
            )
            .route(
                &ApiRoutes::OwnedUtxos.with_arg_template("owner"),
                web::get().to(query_owned_utxos),
            )
            .route(
                &ApiRoutes::ValidatorList.route(),
                web::get().to(query_validators),
            )
            .route(
                &ApiRoutes::DelegationInfo.with_arg_template("XfrPublicKey"),
                web::get().to(query_delegation_info),
            )
            .route(
                &ApiRoutes::DelegatorList.with_arg_template("NodeAddress"),
                web::get().to(query_delegator_list),
            )
            .service(
                web::resource("/delegator_list")
                    .route(web::get().to(get_delegators_with_params)),
            )
            .service(
                web::resource("/delegation_rewards")
                    .route(web::get().to(get_delegation_reward)),
            )
            .service(
                web::resource("/validator_delegation")
                    .route(web::get().to(get_validator_delegation_history)),
            )
            .route(
                &ApiRoutes::ValidatorDetail.with_arg_template("NodeAddress"),
                web::get().to(query_validator_detail),
            )
    }
}

impl RestfulApiService {
    pub fn create(
        ledger_access: Arc<RwLock<LedgerState>>,
        host: &str,
        port: u16,
    ) -> Result<RestfulApiService> {
        let _ = actix_rt::System::new("findora API");

        HttpServer::new(move || {
            App::new()
                .wrap(middleware::Logger::default())
                .wrap(Cors::permissive().supports_credentials())
                .data(ledger_access.clone())
                .set_route()
        })
        .bind(&format!("{}:{}", host, port))
        .c(d!())?
        .run();

        info!("RestfulApi server started");

        Ok(RestfulApiService)
    }
}
