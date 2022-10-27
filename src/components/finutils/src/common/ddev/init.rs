use super::Env;
use crate::{
    common::{
        self,
        ddev::{IDX_APP_8668, IDX_APP_8669, IDX_TM_RPC},
        utils::gen_transfer_op_xx,
    },
    txn_builder::TransactionBuilder,
};
use globutils::{HashOf, SignatureOf};
use ledger::{
    data_model::{
        AssetTypeCode, StateCommitmentData, Transaction, BLACK_HOLE_PUBKEY_STAKING,
    },
    staking::{td_addr_to_bytes, Validator as StakingValidator, ValidatorKind, FRA},
    store::utils::fra_gen_initial_tx,
};
use ruc::*;
use serde::{Deserialize, Serialize};
use zei::xfr::{
    asset_record::AssetRecordType,
    sig::{XfrKeyPair, XfrPublicKey, XfrSecretKey},
};

#[derive(Deserialize)]
struct TmValidators {
    result: TmValidatorsRet,
}

#[derive(Deserialize)]
struct TmValidatorsRet {
    validators: Vec<TmValidator>,
}

#[derive(Deserialize)]
struct TmValidator {
    address: String,
    pub_key: TmPubKey,
}

#[derive(Deserialize)]
struct TmPubKey {
    value: String,
}

pub(super) fn init(env: &mut Env) -> Result<()> {
    let (addr, ports) = env.get_addrports_any_node();
    let port = ports[IDX_TM_RPC];
    let page_size = env.custom_data.initial_validator_num;
    let tmrpc_endpoint =
        format!("http://{}:{}/validators?per_page={}", addr, port, page_size);

    let tm_validators = attohttpc::get(&tmrpc_endpoint)
        .send()
        .c(d!(tmrpc_endpoint))?
        .error_for_status()
        .c(d!())?
        .bytes()
        .c(d!())
        .and_then(|b| serde_json::from_slice::<TmValidators>(&b).c(d!()))?;

    tm_validators.result.validators.into_iter().for_each(|v| {
        let xfr_key = common::gen_key();
        let iv = InitialValidator {
            tendermint_addr: v.address,
            tendermint_pubkey: v.pub_key.value,
            xfr_keypair: xfr_key.3,
            xfr_mnemonic: xfr_key.1,
            xfr_wallet_addr: xfr_key.0,
        };
        env.custom_data.initial_validators.push(iv);
    });

    setup_initial_validators(env).c(d!())?;

    macro_rules! sleep_n_block {
        ($n_block: expr) => {{
            let n = $n_block as f64;
            let mut itv = f64::from(env.block_itv_secs);
            alt!(itv < 2.0, itv = 2.0);
            sleep_ms!((n * itv * 1000.0) as u64);
        }};
    }

    let root_kp = serde_json::from_str::<XfrSecretKey>(&format!(
        "\"{}\"",
        BankAccount::BANK_ACCOUNT_SECKEY
    ))
    .c(d!())?
    .into_keypair();
    println!(
        "[ {} ] >>> Block interval: {} seconds",
        &env.name, env.block_itv_secs
    );

    println!("[ {} ] >>> Define and issue FRA ...", &env.name);
    send_tx(env, &fra_gen_initial_tx(&root_kp)).c(d!())?;

    println!("[ {} ] >>> Wait 2 block ...", &env.name);
    sleep_n_block!(2);

    let target_list = env
        .custom_data
        .initial_validators
        .iter()
        .map(|v| (v.xfr_keypair.get_pk_ref(), 500_0000 * FRA))
        .collect::<Vec<_>>();

    println!("[ {} ] >>> Transfer FRAs to validators ...", &env.name);
    transfer_batch(env, &root_kp, target_list, None, true, true).c(d!())?;

    println!("[ {} ] >>> Wait 2 block ...", &env.name);
    sleep_n_block!(2);

    println!("[ {} ] >>> Propose self-delegations ...", &env.name);
    for (i, v) in env.custom_data.initial_validators.iter().enumerate() {
        let mut builder = new_tx_builder(env).c(d!())?;
        let am = (400_0000 + i as u64 * 1_0000) * FRA;
        gen_transfer_op_xx(
            Some(&gen_8668_endpoint(env)),
            &v.xfr_keypair,
            vec![(&BLACK_HOLE_PUBKEY_STAKING, am)],
            None,
            true,
            false,
            false,
            Some(AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType),
        )
        .c(d!())
        .map(|principal_op| {
            builder.add_operation(principal_op);
            builder.add_operation_delegation(
                &v.xfr_keypair,
                am,
                v.tendermint_addr.clone(),
            );
        })?;
        let mut tx = builder.take_transaction();
        tx.sign(&v.xfr_keypair);
        send_tx(env, &tx).c(d!())?;
    }

    println!("[ {} ] >>> Init work done !", &env.name);
    Ok(())
}

fn setup_initial_validators(env: &Env) -> Result<()> {
    let mut builder = new_tx_builder(env).c(d!())?;

    let vs = env
        .custom_data
        .initial_validators
        .iter()
        .map(StakingValidator::from)
        .collect::<Vec<_>>();
    builder.add_operation_update_validator(&[], 1, vs).c(d!())?;

    send_tx(env, &builder.take_transaction()).c(d!())
}

fn send_tx(env: &Env, tx: &Transaction) -> Result<()> {
    let (addr, ports) = env.get_addrports_any_node();
    let port = ports[IDX_APP_8669];
    let rpc_endpoint = format!("http://{}:{}/submit_transaction", addr, port);
    attohttpc::post(&rpc_endpoint)
        .header(attohttpc::header::CONTENT_TYPE, "application/json")
        .bytes(&serde_json::to_vec(tx).c(d!())?)
        .send()
        .c(d!(rpc_endpoint))?
        .error_for_status()
        .c(d!())
        .map(|_| ())
}

fn transfer_batch(
    env: &Env,
    owner_kp: &XfrKeyPair,
    target_list: Vec<(&XfrPublicKey, u64)>,
    token_code: Option<AssetTypeCode>,
    confidential_am: bool,
    confidential_ty: bool,
) -> Result<()> {
    let mut builder = new_tx_builder(env).c(d!())?;
    let op = gen_transfer_op_xx(
        Some(&gen_8668_endpoint(env)),
        owner_kp,
        target_list,
        token_code,
        true,
        confidential_am,
        confidential_ty,
        None,
    )
    .c(d!())?;
    builder.add_operation(op);

    let mut tx = builder.take_transaction();
    tx.sign(owner_kp);

    send_tx(env, &tx).c(d!())
}

fn new_tx_builder(env: &Env) -> Result<TransactionBuilder> {
    type Resp = (
        HashOf<Option<StateCommitmentData>>,
        u64,
        SignatureOf<(HashOf<Option<StateCommitmentData>>, u64)>,
    );

    let rpc_endpoint = format!("{}/global_state", gen_8668_endpoint(env));

    attohttpc::get(&rpc_endpoint)
        .send()
        .c(d!(rpc_endpoint))?
        .error_for_status()
        .c(d!())?
        .bytes()
        .c(d!())
        .and_then(|b| serde_json::from_slice::<Resp>(&b).c(d!()))
        .map(|resp| resp.1)
        .map(TransactionBuilder::from_seq_id)
}

fn gen_8668_endpoint(env: &Env) -> String {
    let (addr, ports) = env.get_addrports_any_node();
    let port = ports[IDX_APP_8668];
    format!("http://{}:{}", addr, port)
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub(crate) struct InitialValidator {
    tendermint_addr: String,
    tendermint_pubkey: String,

    xfr_keypair: XfrKeyPair,
    xfr_mnemonic: String,
    xfr_wallet_addr: String,
}

impl From<&InitialValidator> for StakingValidator {
    fn from(v: &InitialValidator) -> StakingValidator {
        StakingValidator {
            td_pubkey: base64::decode(&v.tendermint_pubkey).unwrap(),
            td_addr: td_addr_to_bytes(&v.tendermint_addr).unwrap(),
            td_power: 400_0000 * FRA,
            commission_rate: [1, 100],
            id: v.xfr_keypair.get_pk(),
            memo: Default::default(),
            kind: ValidatorKind::Initiator,
            signed_last_block: false,
            signed_cnt: 0,
            delegators: Default::default(),
        }
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub(crate) struct BankAccount {
    wallet_address: String,
    public_key: String,
    secret_key: String,
    mnemonic_words: String,
}

impl BankAccount {
    const BANK_ACCOUNT_ADDR: &str =
        "fra18xkez3fum44jq0zhvwq380rfme7u624cccn3z56fjeex6uuhpq6qv9e4g5";
    const BANK_ACCOUNT_PUBKEY: &str = "Oa2RRTzdayA8V2OBE7xp3n3NKrjGJxFTSZZybXOXCDQ=";
    const BANK_ACCOUNT_SECKEY: &str = "Ew9fMaryTL44ZXnEhcF7hQ-AB-fxgaC8vyCH-hCGtzg=";
    const BANK_ACCOUNT_MNEMONIC: &str = "field ranch pencil chest effort coyote april move injury illegal forest amount bid sound mixture use second pet embrace twice total essay valve loan";
}

impl Default for BankAccount {
    fn default() -> Self {
        Self {
            wallet_address: Self::BANK_ACCOUNT_ADDR.to_owned(),
            public_key: Self::BANK_ACCOUNT_PUBKEY.to_owned(),
            secret_key: Self::BANK_ACCOUNT_SECKEY.to_owned(),
            mnemonic_words: Self::BANK_ACCOUNT_MNEMONIC.to_owned(),
        }
    }
}
