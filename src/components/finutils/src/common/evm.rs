//!
//! EVM transfer functions
//!

use super::get_keypair;
use super::get_serv_addr;
use super::utils;
use fp_core::account::SmartAccount;
use fp_types::{
    actions::{
        xhub::{
            Action as AccountAction, NonConfidentialOutput, NonConfidentialTransfer,
        },
        Action,
    },
    assemble::{CheckFee, CheckNonce},
    crypto::{Address, MultiSignature, MultiSigner},
    transaction::UncheckedTransaction,
    U256,
};
use fp_utils::ecdsa::SecpPair;
use fp_utils::tx::EvmRawTxWrapper;
use ledger::data_model::AssetTypeCode;
use ledger::data_model::ASSET_TYPE_FRA;
use ledger::data_model::BLACK_HOLE_PUBKEY_STAKING;
use ruc::*;
use std::str::FromStr;
use tendermint::block::Height;
use tendermint_rpc::endpoint::abci_query::AbciQuery;
use tendermint_rpc::{Client, HttpClient};
use tokio::runtime::Runtime;
use zei::noah_api::xfr::asset_record::AssetRecordType;
use zei::{XfrKeyPair, XfrPublicKey};

/// transfer utxo assets to account(ed25519 or ecdsa address) balance.
pub fn transfer_to_account(
    amount: u64,
    address: Option<&str>,
    asset: Option<&str>,
    lowlevel_data: Option<&str>,
) -> Result<()> {
    let mut builder = utils::new_tx_builder().c(d!())?;

    let kp = get_keypair().c(d!())?;

    let asset = if let Some(asset) = asset {
        let asset = AssetTypeCode::new_from_base64(asset)?;
        Some(asset)
    } else {
        None
    };

    let transfer_op = utils::gen_transfer_op(
        &kp,
        vec![(XfrPublicKey::from_noah(&BLACK_HOLE_PUBKEY_STAKING), amount)],
        asset,
        false,
        false,
        Some(AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType),
    )
    .c(d!())?;
    let target_address = match address {
        Some(s) => MultiSigner::from_str(s).c(d!())?,
        None => MultiSigner::Xfr(kp.get_pk()),
    };

    let lowlevel_data = if let Some(data) = lowlevel_data {
        let data = hex::decode(data).c(d!())?;
        Some(data)
    } else {
        None
    };

    builder
        .add_operation(transfer_op)
        .add_operation_convert_account(&kp, target_address, amount, asset, lowlevel_data)
        .c(d!())?
        .sign(&kp);

    let mut tx = builder.take_transaction();
    tx.sign_to_map(&kp);

    utils::send_tx(&tx).c(d!())
}

#[allow(missing_docs)]
pub enum Keypair {
    Ed25519(XfrKeyPair),
    Ecdsa(SecpPair),
}

#[allow(missing_docs)]
impl Keypair {
    pub fn sign(&self, data: &[u8]) -> MultiSignature {
        match self {
            Keypair::Ecdsa(kp) => MultiSignature::from(kp.sign(data)),
            Keypair::Ed25519(kp) => {
                MultiSignature::from(kp.get_sk_ref().sign(data).unwrap())
            }
        }
    }
}

/// transfer to uxto assets from account(ed25519 or ecdsa address) balance.
pub fn transfer_from_account(
    amount: u64,
    address: Option<&str>,
    eth_phrase: Option<&str>,
) -> Result<()> {
    let fra_kp = get_keypair()?;

    let target = match address {
        Some(s) => {
            if let Ok(address) = globutils::wallet::public_key_from_base64(s) {
                address
            } else {
                globutils::wallet::public_key_from_bech32(s)?
            }
        }
        None => fra_kp.get_pk(),
    };

    let output = NonConfidentialOutput {
        target,
        amount,
        asset: ASSET_TYPE_FRA,
        decimal: 6,
        max_supply: 0,
    };

    let (signer, kp) = if let Some(key_path) = eth_phrase {
        let kp = SecpPair::from_phrase(key_path, None)?.0;
        let signer = Address::from(kp.address());
        (signer, Keypair::Ecdsa(kp))
    } else {
        let signer = Address::from(fra_kp.get_pk());
        (signer, Keypair::Ed25519(fra_kp))
    };

    let tm_client = tendermint_rpc::HttpClient::new(
        format!("{}:26657", get_serv_addr().c(d!())?).as_str(),
    )
    .unwrap();

    let query_ret = one_shot_abci_query(
        &tm_client,
        "module/account/nonce",
        serde_json::to_vec(&signer).unwrap(),
        None,
        false,
    )?;

    let nonce = serde_json::from_slice::<U256>(query_ret.value.as_slice())
        .c(d!("invalid nonce"))?;

    let account_call = AccountAction::NonConfidentialTransfer(NonConfidentialTransfer {
        input_value: amount,
        outputs: vec![output],
    });
    let action = Action::XHub(account_call);
    let extra = (CheckNonce::new(nonce), CheckFee::new(None));
    let msg = serde_json::to_vec(&(action.clone(), extra.clone())).unwrap();

    let signature = kp.sign(msg.as_slice());

    let tx = UncheckedTransaction::new_signed(action, signer, signature, extra);
    let txn = serde_json::to_vec(&tx).unwrap();

    let txn_with_tag = EvmRawTxWrapper::wrap(&txn);

    Runtime::new()
        .unwrap()
        .block_on(tm_client.broadcast_tx_sync(txn_with_tag.into()))
        .c(d!())?;

    Ok(())
}

fn one_shot_abci_query(
    tm_client: &HttpClient,
    path: &str,
    data: Vec<u8>,
    height: Option<Height>,
    prove: bool,
) -> Result<AbciQuery> {
    let path = if path.is_empty() {
        None
    } else {
        Some(tendermint::abci::Path::from_str(path).unwrap())
    };

    let query_ret = Runtime::new()
        .c(d!())?
        .block_on(tm_client.abci_query(path, data, height, prove))
        .c(d!("abci query error"))?;

    if query_ret.code.is_err() {
        Err(eg!(format!(
            "error code: {:?}, log: {}",
            query_ret.code, query_ret.log
        )))
    } else {
        Ok(query_ret)
    }
}

/// Query contract account info by abci/query
pub fn contract_account_info(address: Option<&str>) -> Result<(Address, SmartAccount)> {
    let fra_kp = get_keypair()?;

    let address = match address {
        Some(s) => MultiSigner::from_str(s).c(d!())?,
        None => MultiSigner::Xfr(fra_kp.get_pk()),
    };
    let account: Address = address.into();

    let tm_client = tendermint_rpc::HttpClient::new(
        format!("{}:26657", get_serv_addr().c(d!())?).as_str(),
    )
    .unwrap();

    let query_ret = one_shot_abci_query(
        &tm_client,
        "module/account/info",
        serde_json::to_vec(&account).unwrap(),
        None,
        false,
    )?;

    Ok((
        account,
        serde_json::from_slice(query_ret.value.as_slice())
            .c(d!("invalid account info"))?,
    ))
}
