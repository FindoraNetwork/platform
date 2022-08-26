//!
//! EVM transfer functions
//!

use super::get_keypair;
use super::get_serv_addr;
use super::utils;
use fp_core::account::SmartAccount;
use fp_types::crypto::IdentifyAccount;
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
    H160, U256,
};
use fp_utils::ecdsa::SecpPair;
use fp_utils::tx::EvmRawTxWrapper;
use ledger::data_model::ASSET_TYPE_FRA;
use ledger::data_model::BLACK_HOLE_PUBKEY_STAKING;
use ledger::data_model::{AssetTypeCode, BLACK_HOLE_PUBKEY};
use ledger::staking::FRA;
use ruc::*;
use std::str::FromStr;
use tendermint::block::Height;
use tendermint_rpc::endpoint::abci_query::AbciQuery;
use tendermint_rpc::{Client, HttpClient};
use tokio::runtime::Runtime;
use zei::xfr::{asset_record::AssetRecordType, sig::XfrKeyPair};

/// transfer utxo assets to account(ed25519 or ecdsa address) balance.
pub fn transfer_to_account(
    amount: u64,
    asset: Option<&str>,
    address: Option<&str>,
    lowlevel_data: Option<&str>,
    deploy: Option<&str>,
) -> Result<()> {
    let mut builder = utils::new_tx_builder()?;

    let kp = get_keypair()?;

    let asset = if let Some(asset) = asset {
        let asset = AssetTypeCode::new_from_base64(asset)?;
        Some(asset)
    } else {
        None
    };

    let lowlevel_data = if let Some(data) = lowlevel_data {
        let data = hex::decode(data).c(d!())?;
        Some(data)
    } else {
        None
    };

    let addr = get_serv_addr()?;
    let web3_addr = format!("{}:8545", addr);

    let (target_address, to) = if deploy.is_none() {
        match address {
            Some(s) => {
                let ms = MultiSigner::from_str(s).c(d!())?;
                let address = ms.clone().into_account();
                let bytes: &[u8] = address.as_ref();
                (ms, Some(bytes[4..24].to_vec()))
            }
            None => {
                let ms = MultiSigner::Xfr(kp.get_pk());
                (ms, None)
            }
        }
    } else {
        let create = H160::zero();
        let ms = MultiSigner::Ethereum(create);
        (ms, None)
    };

    let rt = Runtime::new().c(d!())?;
    rt.block_on(async {
        let gas_price = utils::get_gas_price(web3_addr.as_str()).await.unwrap();
        let gas_limit = utils::get_gas_limit(
            web3_addr.as_str(),
            lowlevel_data.clone(),
            to,
            Some(gas_price),
        )
        .await
        .unwrap();

        let gas_price_real = gas_price.as_u64().saturating_div(FRA);

        let fee = gas_price_real * gas_limit.as_u64();
        println!(
            "gas price, gas limit, total gas: {}, {:?}, {}",
            gas_price_real, gas_limit, fee
        );

        let transfer_op = utils::gen_transfer_op(
            &kp,
            vec![
                (&BLACK_HOLE_PUBKEY_STAKING, amount),
                (&BLACK_HOLE_PUBKEY, fee),
            ],
            asset,
            false,
            false,
            Some(AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType),
        )
        .unwrap();

        builder
            .add_operation(transfer_op)
            .add_operation_convert_account(
                &kp,
                target_address,
                asset,
                amount,
                lowlevel_data,
                gas_price,
                gas_limit,
            )
            .unwrap()
            .sign(&kp);

        let mut tx = builder.build_and_take_transaction().unwrap();

        tx.sign(&kp);

        utils::send_tx(&tx).unwrap();
    });

    Ok(())
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
                MultiSignature::from(kp.get_sk_ref().sign(data, kp.get_pk_ref()))
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
