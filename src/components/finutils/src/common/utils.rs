//!
//! Some handful function and data structure for findora cli tools
//!

use std::collections::BTreeMap;

use {
    crate::{
        api::{DelegationInfo, ValidatorDetail},
        common::get_serv_addr,
        txn_builder::{TransactionBuilder, TransferOperationBuilder},
    },
    globutils::{wallet, HashOf, SignatureOf},
    ledger::{
        data_model::{
            AssetType, AssetTypeCode, DefineAsset, Operation, StateCommitmentData,
            Transaction, TransferType, TxoRef, TxoSID, Utxo, ASSET_TYPE_FRA,
            BLACK_HOLE_PUBKEY, TX_FEE_MIN,
        },
        staking::{init::get_inital_validators, TendermintAddrRef, FRA_TOTAL_AMOUNT},
    },
    ruc::*,
    serde::{self, Deserialize, Serialize},
    sha2::{Digest, Sha256},
    std::collections::HashMap,
    tendermint::{PrivateKey, PublicKey},
    zei::xfr::{
        asset_record::{open_blind_asset_record, AssetRecordType},
        sig::{XfrKeyPair, XfrPublicKey},
        structs::{AssetRecordTemplate, OwnerMemo},
    },
};

///////////////////////////////////////
// Part 1: utils for transfer assets //
///////////////////////////////////////

#[inline(always)]
#[allow(missing_docs)]
pub fn new_tx_builder() -> Result<TransactionBuilder> {
    get_seq_id().c(d!()).map(TransactionBuilder::from_seq_id)
}

#[inline(always)]
#[allow(missing_docs)]
pub fn send_tx(tx: &Transaction) -> Result<()> {
    let url = format!("{}:8669/submit_transaction", get_serv_addr().c(d!())?);
    let tx_bytes = serde_json::to_vec(tx).c(d!())?;

    let ret = attohttpc::post(url)
        .header(attohttpc::header::CONTENT_TYPE, "application/json")
        .bytes(&tx_bytes)
        .send()
        .c(d!("fail to send transaction"))?
        .error_for_status()
        .c(d!())
        .map(|_| ());

    let tx_hash = Sha256::digest(tx_bytes);
    println!("{}", hex::encode(tx_hash));

    ret
}

/// Fee is needless in a `UpdateValidator` operation
#[inline(always)]
pub fn set_initial_validators() -> Result<()> {
    let mut builder = new_tx_builder().c(d!())?;

    let vs = get_inital_validators().c(d!())?;
    builder.add_operation_update_validator(&[], 1, vs).c(d!())?;

    send_tx(&builder.take_transaction()).c(d!())
}

#[inline(always)]
#[allow(missing_docs)]
pub fn transfer(
    owner_kp: &XfrKeyPair,
    target_pk: &XfrPublicKey,
    am: u64,
    token_code: Option<AssetTypeCode>,
    confidential_am: bool,
    confidential_ty: bool,
) -> Result<()> {
    // FRA asset is the default case
    if token_code.is_none() && FRA_TOTAL_AMOUNT < am {
        return Err(eg!("Requested amount exceeds limit!"));
    } else if token_code.is_some() {
        // TODO: need more checking for a custom asset
    }
    transfer_batch(
        owner_kp,
        vec![(target_pk, am)],
        token_code,
        confidential_am,
        confidential_ty,
    )
    .c(d!())
}

#[inline(always)]
#[allow(missing_docs)]
pub fn transfer_batch(
    owner_kp: &XfrKeyPair,
    target_list: Vec<(&XfrPublicKey, u64)>,
    token_code: Option<AssetTypeCode>,
    confidential_am: bool,
    confidential_ty: bool,
) -> Result<()> {
    let mut builder = new_tx_builder().c(d!())?;
    let op = gen_transfer_op(
        owner_kp,
        target_list,
        token_code,
        confidential_am,
        confidential_ty,
        None,
    )
    .c(d!())?;
    builder.add_operation(op);

    let mut tx = builder.take_transaction();
    tx.sign_to_map(owner_kp);

    send_tx(&tx).c(d!())
}

/// @target_list: use `Vec` but `HashMap` ?
///     there might be multi entries to one address
#[inline(always)]
pub fn gen_transfer_op(
    owner_kp: &XfrKeyPair,
    target_list: Vec<(&XfrPublicKey, u64)>,
    token_code: Option<AssetTypeCode>,
    confidential_am: bool,
    confidential_ty: bool,
    balance_type: Option<AssetRecordType>,
) -> Result<Operation> {
    gen_transfer_op_x(
        owner_kp,
        target_list,
        token_code,
        true,
        confidential_am,
        confidential_ty,
        balance_type,
    )
    .c(d!())
}

#[allow(missing_docs)]
pub fn gen_transfer_op_x(
    owner_kp: &XfrKeyPair,
    target_list: Vec<(&XfrPublicKey, u64)>,
    token_code: Option<AssetTypeCode>,
    auto_fee: bool,
    confidential_am: bool,
    confidential_ty: bool,
    balance_type: Option<AssetRecordType>,
) -> Result<Operation> {
    gen_transfer_op_xx(
        None,
        owner_kp,
        target_list,
        token_code,
        auto_fee,
        confidential_am,
        confidential_ty,
        balance_type,
    )
    .c(d!())
}

#[allow(missing_docs)]
#[allow(clippy::too_many_arguments)]
pub fn gen_transfer_op_xx(
    rpc_endpoint: Option<&str>,
    owner_kp: &XfrKeyPair,
    mut target_list: Vec<(&XfrPublicKey, u64)>,
    token_code: Option<AssetTypeCode>,
    auto_fee: bool,
    confidential_am: bool,
    confidential_ty: bool,
    balance_type: Option<AssetRecordType>,
) -> Result<Operation> {
    let mut op_fee: u64 = 0;
    if auto_fee {
        target_list.push((&*BLACK_HOLE_PUBKEY, TX_FEE_MIN));
        op_fee += TX_FEE_MIN;
    }
    let asset_type = token_code.map(|code| code.val).unwrap_or(ASSET_TYPE_FRA);

    let mut trans_builder = TransferOperationBuilder::new();

    let mut am = target_list.iter().map(|(_, am)| *am).sum();
    if asset_type != ASSET_TYPE_FRA {
        am -= op_fee;
    } else {
        // if this is a FRA asset, set op_fee to 0, because fee has been added to am already.
        op_fee = 0;
    }
    let mut i_am;
    let utxos = get_owned_utxos_x(rpc_endpoint, owner_kp.get_pk_ref())
        .c(d!())?
        .into_iter();

    for (sid, (utxo, owner_memo)) in utxos {
        let oar =
            open_blind_asset_record(&utxo.0.record, &owner_memo, owner_kp).c(d!())?;

        if oar.asset_type != asset_type && oar.asset_type != ASSET_TYPE_FRA {
            continue;
        } else if oar.asset_type == ASSET_TYPE_FRA && op_fee != 0 {
            // asset_type is a custom asset, need handle fee here
            alt!(oar.amount < op_fee, i_am = oar.amount, i_am = op_fee);
            op_fee -= i_am;

            trans_builder
                .add_input(TxoRef::Absolute(sid), oar, None, None, i_am)
                .c(d!())?;

            continue;
        } else if am != 0 {
            alt!(oar.amount < am, i_am = oar.amount, i_am = am);
            //am = am.saturating_sub(i_am);
            am -= i_am;

            trans_builder
                .add_input(TxoRef::Absolute(sid), oar, None, None, i_am)
                .c(d!())?;
        }

        alt!(0 == am && 0 == op_fee, break);
    }

    if 0 != am || 0 != op_fee {
        return Err(eg!("insufficient balance"));
    }

    if auto_fee {
        target_list.pop();
        trans_builder
            .add_output(
                &AssetRecordTemplate::with_no_asset_tracing(
                    TX_FEE_MIN,
                    ASSET_TYPE_FRA,
                    AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                    *BLACK_HOLE_PUBKEY,
                ),
                None,
                None,
                None,
            )
            .c(d!())?;
    }

    let art = match (confidential_am, confidential_ty) {
        (true, true) => AssetRecordType::ConfidentialAmount_ConfidentialAssetType,
        (true, false) => AssetRecordType::ConfidentialAmount_NonConfidentialAssetType,
        (false, true) => AssetRecordType::NonConfidentialAmount_ConfidentialAssetType,
        _ => AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
    };

    let outputs = target_list.into_iter().map(|(pk, n)| {
        AssetRecordTemplate::with_no_asset_tracing(
            n,
            token_code.map(|code| code.val).unwrap_or(ASSET_TYPE_FRA),
            art,
            *pk,
        )
    });

    for output in outputs {
        trans_builder
            .add_output(&output, None, None, None)
            .c(d!())?;
    }

    trans_builder
        .balance(balance_type)
        .c(d!())?
        .create(TransferType::Standard)
        .c(d!())?
        .sign(owner_kp)
        .c(d!())?
        .transaction()
        .c(d!())
}

/// for scenes that need to pay a standalone fee without other transfers
#[inline(always)]
#[allow(missing_docs)]
pub fn gen_fee_op(owner_kp: &XfrKeyPair) -> Result<Operation> {
    gen_transfer_op(owner_kp, vec![], None, false, false, None).c(d!())
}

/////////////////////////////////////////
// Part 2: utils for query infomations //
/////////////////////////////////////////

#[derive(Serialize, Deserialize, Debug)]
// tendermint status repsonse, "Tm" is short for "tendermint"
// see also: https://docs.tendermint.com/master/rpc/#/Info/status
struct TmStatusResp {
    jsonrpc: String,
    id: i32,
    result: TmStatus,
}

// The protocol version of current tendermint node
#[derive(Serialize, Deserialize, Debug)]
struct TmProtoVersion {
    p2p: String,
    block: String,
    app: String,
}

// The extra info of current tendermint node
//     tx_index - if enable indexer for transaction, on or off
//     rpc_address - the TCP or UNIX socket for the rpc server to listen on
#[derive(Serialize, Deserialize, Debug)]
struct TmOtherInfo {
    tx_index: String,
    rpc_address: String,
}

// The info of current tendermint node
#[derive(Serialize, Deserialize, Debug)]
struct TmNodeInfo {
    protocol_version: TmProtoVersion,
    id: String,
    // P2P listen address
    listen_addr: String,
    network: String,
    version: String,
    channels: String,
    moniker: String,
    other: TmOtherInfo,
}

// The syncing info of current tendermint node
#[derive(Serialize, Deserialize, Debug)]
struct TmSyncInfo {
    latest_block_hash: String,
    latest_app_hash: String,
    latest_block_height: String,
    latest_block_time: String,
    earliest_block_hash: String,
    earliest_app_hash: String,
    earliest_block_height: String,
    earliest_block_time: String,
    catching_up: bool,
}

// The Validator public key of current tendermint node
#[derive(Serialize, Deserialize, Debug)]
struct TmValidatorPubKey {
    #[serde(rename = "type")]
    pk_type: String,
    value: String,
}

// The Validator info of current tendermint node
#[derive(Serialize, Deserialize, Debug)]
struct TmValidatorInfo {
    // Tendermint Address
    address: String,
    pub_key: TmValidatorPubKey,
    voting_power: String,
}

#[derive(Serialize, Deserialize, Debug)]
struct TmStatus {
    node_info: TmNodeInfo,
    sync_info: TmSyncInfo,
    validator_info: TmValidatorInfo,
}

// retrieve tendermint status and node info
#[inline(always)]
fn get_network_status(addr: &str) -> Result<TmStatus> {
    let url = format!("{addr}:26657/status");

    attohttpc::get(url)
        .send()
        .c(d!())?
        .error_for_status()
        .c(d!())?
        .bytes()
        .c(d!())
        .and_then(|b| {
            serde_json::from_slice::<TmStatusResp>(&b)
                .map(|r| r.result)
                .c(d!())
        })
}

/// Retrieve current block height of the specified tendermint node address
pub fn get_block_height(addr: &str) -> u64 {
    get_network_status(addr)
        .map(|ts| ts.sync_info.latest_block_height.parse::<u64>().unwrap())
        .unwrap_or(0)
}

/// Retrieve current block height of the local tendermint node address
pub fn get_local_block_height() -> u64 {
    let addr = "http://127.0.0.1";
    get_block_height(addr)
}

/// Retrieve custom asset(aka token) type of a findora network with asset code
pub fn get_asset_type(code: &str) -> Result<AssetType> {
    let url = format!("{}:8668/asset_token/{}", get_serv_addr().c(d!())?, code);

    attohttpc::get(url)
        .send()
        .c(d!())?
        .error_for_status()
        .c(d!())?
        .bytes()
        .c(d!())
        .and_then(|b| serde_json::from_slice::<AssetType>(&b).c(d!()))
}

/// Retrieve a list of assets created by the specified findora account
pub fn get_created_assets(
    addr: &XfrPublicKey,
) -> Result<Vec<(AssetTypeCode, DefineAsset)>> {
    let url = format!(
        "{}:8667/get_created_assets/{}",
        get_serv_addr().c(d!())?,
        wallet::public_key_to_base64(addr)
    );

    attohttpc::get(url)
        .send()
        .c(d!())?
        .error_for_status()
        .c(d!())?
        .bytes()
        .c(d!())
        .and_then(|b| serde_json::from_slice(&b).c(d!()))
}

#[inline(always)]
#[allow(missing_docs)]
pub fn get_balance(kp: &XfrKeyPair) -> Result<u64> {
    get_asset_balance(kp, None).c(d!())
}

/// Retrieve Utxos of a findora keypair and calcultate the balance of the specified asset
/// FRA is the default asset type
pub fn get_asset_balance(kp: &XfrKeyPair, asset: Option<AssetTypeCode>) -> Result<u64> {
    let asset_type = asset.map(|code| code.val).unwrap_or(ASSET_TYPE_FRA);
    let balance = get_owned_utxos(kp.get_pk_ref())
        .c(d!())?
        .values()
        .map(|(utxo, owner_memo)| {
            open_blind_asset_record(&utxo.0.record, owner_memo, kp)
                .c(d!())
                .map(|obr| alt!(obr.asset_type == asset_type, obr.amount, 0))
        })
        .collect::<Result<Vec<_>>>()
        .c(d!())?
        .iter()
        .sum();

    Ok(balance)
}

/// Retrieve Utxos of a findora keypair and calcultate the balance of the specified asset
/// FRA is the default asset type
pub fn get_asset_all(kp: &XfrKeyPair) -> Result<BTreeMap<AssetTypeCode, u64>> {
    let info = get_owned_utxos(kp.get_pk_ref())?;

    let mut set = BTreeMap::new();

    for (_k, v) in info {
        let res = open_blind_asset_record(&v.0 .0.record, &v.1, kp)?;

        let code = AssetTypeCode {
            val: res.asset_type,
        };

        if let Some(amount) = set.get_mut(&code) {
            *amount += res.amount;
        } else {
            set.insert(code, res.amount);
        }
    }

    Ok(set)
}

fn get_owned_utxos(
    addr: &XfrPublicKey,
) -> Result<HashMap<TxoSID, (Utxo, Option<OwnerMemo>)>> {
    get_owned_utxos_x(None, addr).c(d!())
}

fn get_owned_utxos_x(
    rpc_endpoint: Option<&str>,
    addr: &XfrPublicKey,
) -> Result<HashMap<TxoSID, (Utxo, Option<OwnerMemo>)>> {
    let default_endpoint = format!("{}:8668", get_serv_addr().c(d!())?);
    let url = format!(
        "{}/owned_utxos/{}",
        rpc_endpoint.unwrap_or(default_endpoint.as_str()),
        wallet::public_key_to_base64(addr)
    );

    attohttpc::get(url)
        .send()
        .c(d!())?
        .error_for_status()
        .c(d!())?
        .bytes()
        .c(d!())
        .and_then(|b| {
            serde_json::from_slice::<HashMap<TxoSID, (Utxo, Option<OwnerMemo>)>>(&b)
                .c(d!())
        })
}

#[inline(always)]
fn get_seq_id() -> Result<u64> {
    type Resp = (
        HashOf<Option<StateCommitmentData>>,
        u64,
        SignatureOf<(HashOf<Option<StateCommitmentData>>, u64)>,
    );

    let url = format!("{}:8668/global_state", get_serv_addr().c(d!())?);

    attohttpc::get(url)
        .send()
        .c(d!())?
        .error_for_status()
        .c(d!())?
        .bytes()
        .c(d!())
        .and_then(|b| serde_json::from_slice::<Resp>(&b).c(d!()))
        .map(|resp| resp.1)
}

#[inline(always)]
#[allow(missing_docs)]
pub fn get_owner_memo_batch(ids: &[TxoSID]) -> Result<Vec<Option<OwnerMemo>>> {
    let ids = ids
        .iter()
        .map(|id| id.0.to_string())
        .collect::<Vec<_>>()
        .join(",");
    let url = format!(
        "{}:8667/get_owner_memo_batch/{}",
        get_serv_addr().c(d!())?,
        ids
    );

    attohttpc::get(url)
        .send()
        .c(d!())?
        .error_for_status()
        .c(d!())?
        .bytes()
        .c(d!())
        .and_then(|b| serde_json::from_slice(&b).c(d!()))
}

/// Delegation info(and staking info if `pk` is a validator).
pub fn get_delegation_info(pk: &XfrPublicKey) -> Result<DelegationInfo> {
    let url = format!(
        "{}:8668/delegation_info/{}",
        get_serv_addr().c(d!())?,
        wallet::public_key_to_base64(pk)
    );

    attohttpc::get(url)
        .send()
        .c(d!())?
        .error_for_status()
        .c(d!())?
        .bytes()
        .c(d!())
        .and_then(|b| serde_json::from_slice::<DelegationInfo>(&b).c(d!()))
}

/// Get validator infomations.
pub fn get_validator_detail(td_addr: TendermintAddrRef) -> Result<ValidatorDetail> {
    let url = format!(
        "{}:8668/validator_detail/{}",
        get_serv_addr().c(d!())?,
        td_addr
    );

    attohttpc::get(url)
        .send()
        .c(d!())?
        .error_for_status()
        .c(d!())?
        .bytes()
        .c(d!())
        .and_then(|b| serde_json::from_slice::<ValidatorDetail>(&b).c(d!()))
}

#[allow(missing_docs)]
#[derive(Serialize, Deserialize)]
pub struct ValidatorKey {
    pub address: String,
    pub pub_key: PublicKey,
    pub priv_key: PrivateKey,
}

/// Restore validator key from a string
pub fn parse_td_validator_keys(key_data: &str) -> Result<ValidatorKey> {
    serde_json::from_str(key_data).c(d!())
}
