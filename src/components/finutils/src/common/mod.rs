//!
//! # Findora Network Cli tool
//!
//! FN, a command line tool for findora network.
//!
//! This module is the library part of FN.
//!

use std::str::FromStr;

#[cfg(not(target_arch = "wasm32"))]
pub mod dev;

#[cfg(not(target_arch = "wasm32"))]
pub mod ddev;

pub mod evm;
pub mod utils;

use {
    self::utils::{get_evm_staking_address, get_validator_memo_and_rate},
    crate::{
        api::DelegationInfo,
        common::utils::{mapping_address, new_tx_builder, send_tx},
        txn_builder::TransactionBuilder,
    },
    globutils::wallet,
    lazy_static::lazy_static,
    ledger::{
        data_model::{
            gen_random_keypair, get_abar_commitment, ATxoSID, AssetRules, AssetTypeCode,
            AssetTypePrefix, Transaction, TxoSID, ASSET_TYPE_FRA,
            BLACK_HOLE_PUBKEY_STAKING,
        },
        staking::{
            check_delegation_amount, td_addr_to_bytes, td_pubkey_to_td_addr,
            td_pubkey_to_td_addr_bytes, PartialUnDelegation, StakerMemo,
            TendermintAddrRef,
        },
    },
    rand_chacha::ChaChaRng,
    rand_core::SeedableRng,
    ruc::*,
    std::{env, fs},
    tendermint::PrivateKey,
    utils::{get_block_height, get_local_block_height, parse_td_validator_keys},
    web3::types::H160,
    zei::{
        noah_api::{
            anon_xfr::{
                nullify,
                structs::{
                    AnonAssetRecord, Commitment, MTLeafInfo, OpenAnonAssetRecordBuilder,
                },
            },
            xfr::{
                asset_record::{
                    AssetRecordType,
                    AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
                },
                structs::{XfrAmount, XfrAssetType},
            },
        },
        XfrKeyPair, XfrPublicKey, XfrSecretKey,
    },
};

lazy_static! {
    static ref CFG_PATH: String = format!(
        "{}/.____fn_config____",
        ruc::info!(env::var("HOME")).unwrap_or_else(|_| "/tmp/".to_owned())
    );
    static ref MNEMONIC: Option<String> = fs::read_to_string(&*MNEMONIC_FILE)
        .map(|s| s.trim().to_string())
        .ok();
    static ref MNEMONIC_FILE: String = format!("{}/mnemonic", &*CFG_PATH);
    static ref TD_KEY: Option<String> = fs::read_to_string(&*TD_KEY_FILE).ok();
    static ref TD_KEY_FILE: String = format!("{}/tendermint_keys", &*CFG_PATH);
    static ref SERV_ADDR: Option<String> = fs::read_to_string(&*SERV_ADDR_FILE).ok();
    static ref SERV_ADDR_FILE: String = format!("{}/serv_addr", &*CFG_PATH);
}

/// Updating the information of a staker includes commission_rate and staker_memo
pub fn staker_update(
    cr: Option<&str>,
    memo: Option<StakerMemo>,
    is_address_eth: bool,
) -> Result<()> {
    let pub_key = get_td_pubkey()
        .map(|i| td_pubkey_to_td_addr_bytes(&i))
        .c(d!())?;
    let validator_address = H160::from_slice(&pub_key);

    let evm_staking_address = get_evm_staking_address()?;
    let url = format!("{}:8545", get_serv_addr()?);
    let (validator_memo, rate) =
        get_validator_memo_and_rate(&url, evm_staking_address, validator_address)?;

    let cr = cr
        .map_or(Ok(rate), |s| {
            s.parse::<f64>()
                .c(d!("commission rate must be a float number"))
                .and_then(convert_commission_rate)
        })
        .c(d!())?;
    let memo = memo.unwrap_or(validator_memo);

    let td_pubkey = get_td_pubkey().c(d!())?;

    let kp = get_keypair(is_address_eth).c(d!())?;
    let vkp = get_td_privkey().c(d!())?;

    let mut builder = utils::new_tx_builder().c(d!())?;

    builder
        .add_operation_update_staker(&kp, &vkp, td_pubkey, cr, memo)
        .c(d!())?;
    utils::gen_fee_op(&kp)
        .c(d!())
        .map(|op| builder.add_operation(op))?;

    let mut tx = builder.build_and_take_transaction()?;
    tx.sign_to_map(&kp);

    utils::send_tx(&tx).c(d!())
}

/// Perform a staking operation to add current tendermint node to validator list
/// The cli tool user will be alert if the block height of local node is too small
pub fn stake(
    amount: &str,
    commission_rate: &str,
    memo: Option<&str>,
    force: bool,
    is_address_eth: bool,
) -> Result<()> {
    let am = amount.parse::<u64>().c(d!("'amount' must be an integer"))?;
    check_delegation_amount(am, false).c(d!())?;
    let cr = commission_rate
        .parse::<f64>()
        .c(d!("commission rate must be a float number"))
        .and_then(|cr| convert_commission_rate(cr).c(d!()))?;
    let td_pubkey = get_td_pubkey().c(d!())?;

    let kp = get_keypair(is_address_eth).c(d!())?;
    let vkp = get_td_privkey().c(d!())?;

    macro_rules! diff {
        ($l:expr, $r:expr) => {
            if $l > $r {
                $l - $r
            } else {
                $r - $l
            }
        };
    }

    let network_height = get_block_height(get_serv_addr().unwrap());
    let local_height = get_local_block_height();
    if (network_height == 0 || local_height == 0)
        || diff!(network_height, local_height) > 3
    {
        println!(
            "The difference in block height of your node and the remote network is too big: \n remote / local: {network_height} / {local_height}",
        );
        if !force {
            println!("Append option --force to ignore this warning.");
            return Ok(());
        }
        println!("Continue to stake now...");
    }

    let mut builder = utils::new_tx_builder().c(d!())?;
    builder
        .add_operation_staking(&kp, am, &vkp, td_pubkey, cr, memo.map(|m| m.to_owned()))
        .c(d!())?;
    utils::gen_transfer_op(
        &kp,
        vec![(XfrPublicKey::from_noah(&BLACK_HOLE_PUBKEY_STAKING), am)],
        None,
        false,
        false,
        Some(AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType),
    )
    .c(d!())
    .map(|principal_op| builder.add_operation(principal_op))?;

    let mut tx = builder.build_and_take_transaction()?;
    tx.sign_to_map(&kp);

    utils::send_tx(&tx).c(d!())
}

/// Append more FRA token to the specified tendermint node
pub fn stake_append(
    amount: &str,
    staker: Option<&str>,
    td_addr: Option<TendermintAddrRef>,
    is_address_eth: bool,
) -> Result<()> {
    let am = amount.parse::<u64>().c(d!("'amount' must be an integer"))?;
    check_delegation_amount(am, true).c(d!())?;

    let td_addr = td_addr.map(|ta| ta.to_owned()).c(d!()).or_else(|_| {
        get_td_pubkey()
            .c(d!())
            .map(|td_pk| td_pubkey_to_td_addr(&td_pk))
    })?;

    let kp = staker
        .c(d!())
        .and_then(|sk| wallet::restore_keypair_from_mnemonic_default(sk).c(d!()))
        .or_else(|_| get_keypair(is_address_eth).c(d!()))?;

    let mut builder = utils::new_tx_builder().c(d!())?;
    builder.add_operation_delegation(&kp, am, td_addr);
    utils::gen_transfer_op(
        &kp,
        vec![(XfrPublicKey::from_noah(&BLACK_HOLE_PUBKEY_STAKING), am)],
        None,
        false,
        false,
        Some(AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType),
    )
    .c(d!())
    .map(|principal_op| builder.add_operation(principal_op))?;

    let mut tx = builder.build_and_take_transaction()?;
    tx.sign_to_map(&kp);

    utils::send_tx(&tx).c(d!())
}

/// Withdraw Fra token from findora network for a staker
pub fn unstake(
    am: Option<&str>,
    staker: Option<&str>,
    td_addr: Option<TendermintAddrRef>,
    is_address_eth: bool,
) -> Result<()> {
    let am = if let Some(i) = am {
        Some(i.parse::<u64>().c(d!("'amount' must be an integer"))?)
    } else {
        None
    };

    let kp = staker
        .c(d!())
        .and_then(|sk| wallet::restore_keypair_from_mnemonic_default(sk).c(d!()))
        .or_else(|_| get_keypair(is_address_eth).c(d!()))?;
    let td_addr_bytes = td_addr
        .c(d!())
        .and_then(|ta| td_addr_to_bytes(ta).c(d!()))
        .or_else(|_| {
            get_td_pubkey()
                .c(d!())
                .map(|td_pk| td_pubkey_to_td_addr_bytes(&td_pk))
        })?;

    let mut builder = utils::new_tx_builder().c(d!())?;

    utils::gen_fee_op(&kp).c(d!()).map(|op| {
        builder.add_operation(op);
        if let Some(am) = am {
            // partial undelegation
            builder.add_operation_undelegation(
                &kp,
                Some(PartialUnDelegation::new(
                    am,
                    gen_random_keypair().get_pk(),
                    td_addr_bytes,
                )),
            );
        } else {
            builder.add_operation_undelegation(&kp, None);
        }
    })?;

    let mut tx = builder.build_and_take_transaction()?;
    tx.sign_to_map(&kp);

    utils::send_tx(&tx).c(d!())
}

/// Claim rewards from findora network
pub fn claim(
    td_addr: &str,
    am: Option<&str>,
    sk_str: Option<&str>,
    is_address_eth: bool,
) -> Result<()> {
    let td_addr = hex::decode(td_addr).c(d!())?;

    let am = if let Some(i) = am {
        Some(i.parse::<u64>().c(d!("'amount' must be an integer"))?)
    } else {
        None
    };

    let kp = restore_keypair_from_str_with_default(sk_str, is_address_eth)?;

    let mut builder = utils::new_tx_builder().c(d!())?;

    utils::gen_fee_op(&kp).c(d!()).map(|op| {
        builder.add_operation(op);
        builder.add_operation_claim(Some(td_addr), &kp, am);
    })?;

    let mut tx = builder.build_and_take_transaction()?;
    tx.sign_to_map(&kp);

    utils::send_tx(&tx).c(d!())
}

/// Show information of current node, including following sections:
///     Server URL
///     Findora Wallet Address
///     Findora Public Key
///     Local validator address
///     FRA balance
///     Delegation Information
///     Validator Detail (if already staked)
///
pub fn show(basic: bool, is_address_eth: bool) -> Result<()> {
    let kp = get_keypair(is_address_eth).c(d!())?;

    ruc::info!(get_serv_addr()).map(|i| {
        println!("\x1b[31;01mServer URL:\x1b[00m\n{i}\n");
    })?;

    ruc::info!(get_keypair(is_address_eth)).map(|i| {
        println!(
            "\x1b[31;01mFindora Address:\x1b[00m\n{}\n",
            wallet::public_key_to_bech32(&i.get_pk())
        );
        println!(
            "\x1b[31;01mFindora Public Key:\x1b[00m\n{}\n",
            wallet::public_key_to_base64(&i.get_pk())
        );
        println!(
            "\x1b[31;01mFindora Public Key in hex:\x1b[00m\n{}\n",
            wallet::public_key_to_hex(&i.get_pk())
        );
    })?;

    ruc::info!(utils::get_balance(&kp)).map(|i| {
        println!("\x1b[31;01mNode Balance:\x1b[00m\n{i} FRA units\n");
    })?;

    if basic {
        return Ok(());
    }

    let (_, addr) = ruc::info!(get_td_pubkey()).map(|i| {
        let addr = td_pubkey_to_td_addr(&i);
        println!("\x1b[31;01mValidator Node Addr:\x1b[00m\n{addr}\n");
        (i, addr)
    })?;

    let validator_address = H160::from_str(&addr).c(d!())?;
    let evm_staking_address = get_evm_staking_address()?;
    let url = format!("{}:8545", get_serv_addr()?);
    let address = utils::get_trigger_on_contract_address(&url, evm_staking_address)?;
    let (bound_amount, unbound_amount) = utils::get_evm_delegation_info(
        &url,
        address,
        validator_address,
        mapping_address(kp.get_pk_ref()),
    )?;

    let address = utils::get_claim_on_contract_address(&url, evm_staking_address)?;
    let reward = utils::get_reward_info(
        &url,
        address,
        validator_address,
        mapping_address(kp.get_pk_ref()),
    )?;

    println!(
        "\x1b[31;01mYour Delegation:\x1b[00m\nbound_amount:{:?}\nunbound_amount:{:?}\nreward:{:?}",
        bound_amount, unbound_amount,reward
    );

    Ok(())
}

/// Setup for a cli tool
///    Server URL
///    Owner mnemonic path
///    Tendermint node private key path
pub fn setup(
    serv_addr: Option<&str>,
    owner_mnemonic_path: Option<&str>,
    validator_key_path: Option<&str>,
) -> Result<()> {
    fs::create_dir_all(&*CFG_PATH).c(d!("fail to create config path"))?;

    let mut pwd = ruc::info!(
        env::current_dir(),
        "Cannot abtain current work directory, default to relative path"
    )
    .unwrap_or_default();

    if let Some(sa) = serv_addr {
        fs::write(&*SERV_ADDR_FILE, sa).c(d!("fail to cache 'serv-addr'"))?;
    }
    if let Some(mp) = owner_mnemonic_path {
        let mp = if mp.starts_with('/') {
            mp
        } else {
            pwd.push(mp);
            pwd.to_str().c(d!("Invalid path"))?
        };
        fs::write(&*MNEMONIC_FILE, mp).c(d!("fail to cache 'owner-mnemonic-path'"))?;
    }
    if let Some(kp) = validator_key_path {
        let kp = if kp.starts_with('/') {
            kp
        } else {
            pwd.push(kp);
            pwd.to_str().c(d!("Invalid path"))?
        };
        fs::write(&*TD_KEY_FILE, kp).c(d!("fail to cache 'validator-key-path'"))?;
    }
    Ok(())
}

#[allow(missing_docs)]
pub fn transfer_asset(
    owner_sk: Option<&str>,
    target_addr: XfrPublicKey,
    token_code: Option<AssetTypeCode>,
    am: &str,
    confidential_am: bool,
    confidential_ty: bool,
    is_address_eth: bool,
) -> Result<()> {
    transfer_asset_batch(
        owner_sk,
        &[target_addr],
        token_code,
        am,
        confidential_am,
        confidential_ty,
        is_address_eth,
    )
    .c(d!())
}

#[allow(missing_docs)]
pub fn transfer_asset_x(
    kp: &XfrKeyPair,
    target_addr: XfrPublicKey,
    token_code: Option<AssetTypeCode>,
    am: u64,
    confidential_am: bool,
    confidential_ty: bool,
) -> Result<()> {
    transfer_asset_batch_x(
        kp,
        &[target_addr],
        token_code,
        am,
        confidential_am,
        confidential_ty,
    )
    .c(d!())
}

#[allow(missing_docs)]
pub fn transfer_asset_batch(
    owner_sk: Option<&str>,
    target_addr: &[XfrPublicKey],
    token_code: Option<AssetTypeCode>,
    am: &str,
    confidential_am: bool,
    confidential_ty: bool,
    is_address_eth: bool,
) -> Result<()> {
    let from = restore_keypair_from_str_with_default(owner_sk, is_address_eth)?;
    let am = am.parse::<u64>().c(d!("'amount' must be an integer"))?;

    transfer_asset_batch_x(
        &from,
        target_addr,
        token_code,
        am,
        confidential_am,
        confidential_ty,
    )
    .c(d!())
}

#[allow(missing_docs)]
pub fn transfer_asset_batch_x(
    kp: &XfrKeyPair,
    target_addr: &[XfrPublicKey],
    token_code: Option<AssetTypeCode>,
    am: u64,
    confidential_am: bool,
    confidential_ty: bool,
) -> Result<()> {
    utils::transfer_batch(
        kp,
        target_addr.iter().map(|addr| (*addr, am)).collect(),
        token_code,
        confidential_am,
        confidential_ty,
    )
    .c(d!())
}

/// Mainly for official usage,
/// and can be also used in test scenes.
pub fn set_initial_validators() -> Result<()> {
    utils::set_initial_validators().c(d!())
}

/// Get the effective address of server
pub fn get_serv_addr() -> Result<&'static str> {
    if let Some(sa) = SERV_ADDR.as_ref() {
        Ok(sa)
    } else {
        Err(eg!("'serv-addr' has not been set"))
    }
}

/// Get keypair from config file
pub fn get_keypair(is_address_eth: bool) -> Result<XfrKeyPair> {
    if let Some(m_path) = MNEMONIC.as_ref() {
        fs::read_to_string(m_path)
            .c(d!("can not read mnemonic from 'owner-mnemonic-path'"))
            .and_then(|m| {
                let k = m.trim();
                let kp = if is_address_eth {
                    wallet::restore_keypair_from_mnemonic_secp256k1(k)
                        .c(d!("invalid 'owner-mnemonic'"))
                } else {
                    wallet::restore_keypair_from_mnemonic_default(k)
                        .c(d!("invalid 'owner-mnemonic'"))
                };

                kp.or_else(|e| wallet::restore_keypair_from_seckey_base64(k).c(d!(e)))
            })
    } else {
        Err(eg!("'owner-mnemonic-path' has not been set"))
    }
}

fn get_td_pubkey() -> Result<Vec<u8>> {
    if let Some(key_path) = TD_KEY.as_ref() {
        fs::read_to_string(key_path)
            .c(d!("can not read key file from path"))
            .and_then(|k| {
                let v_keys = parse_td_validator_keys(&k).c(d!())?;
                Ok(v_keys.pub_key.to_vec())
            })
    } else {
        Err(eg!("'validator-pubkey' has not been set"))
    }
}

fn get_td_privkey() -> Result<PrivateKey> {
    if let Some(key_path) = TD_KEY.as_ref() {
        fs::read_to_string(key_path)
            .c(d!("can not read key file from path"))
            .and_then(|k| {
                parse_td_validator_keys(&k)
                    .c(d!())
                    .map(|v_keys| v_keys.priv_key)
            })
    } else {
        Err(eg!("'validator-privkey' has not been set"))
    }
}

#[allow(missing_docs)]
pub fn convert_commission_rate(cr: f64) -> Result<[u64; 2]> {
    if 1.0 < cr {
        return Err(eg!("commission rate can exceed 100%"));
    }
    if 0.0 > cr {
        return Err(eg!("commission rate must be a positive float number"));
    }
    Ok([(cr * 10000.0) as u64, 10000])
}

#[allow(missing_docs)]
pub fn gen_key(is_address_eth: bool) -> (String, String, String, XfrKeyPair) {
    let (mnemonic, key, kp) = loop {
        let mnemonic = pnk!(wallet::generate_mnemonic_custom(24, "en"));
        let kp = if is_address_eth {
            pnk!(wallet::restore_keypair_from_mnemonic_secp256k1(&mnemonic))
        } else {
            pnk!(wallet::restore_keypair_from_mnemonic_default(&mnemonic))
        };

        if let Some(key) = serde_json::to_string_pretty(&kp)
            .ok()
            .filter(|s| s.matches("\": \"-").next().is_none())
        {
            break (mnemonic, key, kp);
        }
    };

    let wallet_addr = wallet::public_key_to_bech32(kp.get_pk_ref());

    (wallet_addr, mnemonic, key, kp)
}

#[allow(missing_docs)]
pub fn gen_key_and_print(is_address_eth: bool) {
    let (wallet_addr, mnemonic, key, _) = gen_key(is_address_eth);
    println!(
        "\n\x1b[31;01mWallet Address:\x1b[00m {wallet_addr}\n\x1b[31;01mMnemonic:\x1b[00m {mnemonic}\n\x1b[31;01mKey:\x1b[00m {key}\n",
    );
}

fn restore_keypair_from_str_with_default(
    sk_str: Option<&str>,
    is_address_eth: bool,
) -> Result<XfrKeyPair> {
    if let Some(sk) = sk_str {
        serde_json::from_str::<XfrSecretKey>(&format!("\"{}\"", sk.trim()))
            .map(|sk| sk.into_keypair())
            .c(d!("Invalid secret key"))
    } else {
        get_keypair(is_address_eth).c(d!())
    }
}

/// Show the asset balance of a findora account
pub fn show_account(
    sk_str: Option<&str>,
    _asset: Option<&str>,
    is_address_eth: bool,
) -> Result<()> {
    let kp = restore_keypair_from_str_with_default(sk_str, is_address_eth)?;
    // let token_code = asset
    //     .map(|asset| AssetTypeCode::new_from_base64(asset).c(d!("Invalid asset code")))
    //     .transpose()?;
    // let balance = utils::get_asset_balance(&kp, token_code).c(d!())?;
    //
    // println!("{}: {}", asset.unwrap_or("FRA"), balance);

    let res = utils::get_asset_all(&kp)?;

    for (k, v) in res {
        let codes = k.to_base64();

        println!("{codes}: {v}");
    }

    Ok(())
}

#[inline(always)]
#[allow(missing_docs)]
pub fn delegate(
    sk_str: Option<&str>,
    amount: u64,
    validator: &str,
    is_address_eth: bool,
) -> Result<()> {
    restore_keypair_from_str_with_default(sk_str, is_address_eth)
        .c(d!())
        .and_then(|kp| delegate_x(&kp, amount, validator).c(d!()))
}

#[inline(always)]
#[allow(missing_docs)]
pub fn delegate_x(kp: &XfrKeyPair, amount: u64, validator: &str) -> Result<()> {
    gen_delegate_tx(kp, amount, validator)
        .c(d!())
        .and_then(|tx| utils::send_tx(&tx).c(d!()))
}

#[inline(always)]
#[allow(missing_docs)]
pub fn undelegate(
    sk_str: Option<&str>,
    param: Option<(u64, &str)>,
    is_address_eth: bool,
) -> Result<()> {
    restore_keypair_from_str_with_default(sk_str, is_address_eth)
        .c(d!())
        .and_then(|kp| undelegate_x(&kp, param).c(d!()))
}

#[inline(always)]
#[allow(missing_docs)]
pub fn undelegate_x(kp: &XfrKeyPair, param: Option<(u64, &str)>) -> Result<()> {
    gen_undelegate_tx(kp, param)
        .c(d!())
        .and_then(|tx| utils::send_tx(&tx).c(d!()))
}

/// Display delegation information of a findora account
pub fn show_delegations(sk_str: Option<&str>, is_address_eth: bool) -> Result<()> {
    let pk = restore_keypair_from_str_with_default(sk_str, is_address_eth)?.get_pk();

    println!(
        "{}",
        serde_json::to_string_pretty::<DelegationInfo>(
            &utils::get_delegation_info(&pk).c(d!())?
        )
        .c(d!())?
    );

    Ok(())
}

fn gen_undelegate_tx(
    owner_kp: &XfrKeyPair,
    param: Option<(u64, &str)>,
) -> Result<Transaction> {
    let mut builder = utils::new_tx_builder().c(d!())?;
    utils::gen_fee_op(owner_kp).c(d!()).map(|op| {
        builder.add_operation(op);
    })?;
    if let Some((amount, validator)) = param {
        // partial undelegation
        builder.add_operation_undelegation(
            owner_kp,
            Some(PartialUnDelegation::new(
                amount,
                gen_random_keypair().get_pk(),
                td_addr_to_bytes(validator).c(d!())?,
            )),
        );
    } else {
        builder.add_operation_undelegation(owner_kp, None);
    }

    let mut tx = builder.build_and_take_transaction()?;
    tx.sign_to_map(owner_kp);

    Ok(tx)
}

fn gen_delegate_tx(
    owner_kp: &XfrKeyPair,
    amount: u64,
    validator: &str,
) -> Result<Transaction> {
    let mut builder = utils::new_tx_builder().c(d!())?;

    utils::gen_transfer_op(
        owner_kp,
        vec![(XfrPublicKey::from_noah(&BLACK_HOLE_PUBKEY_STAKING), amount)],
        None,
        false,
        false,
        Some(AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType),
    )
    .c(d!())
    .map(|principal_op| {
        builder.add_operation(principal_op);
        builder.add_operation_delegation(owner_kp, amount, validator.to_owned());
    })?;

    let mut tx = builder.build_and_take_transaction()?;

    tx.sign_to_map(owner_kp);

    Ok(tx)
}
/// Create a custom asset for a findora account. If no token code string provided,
/// it will generate a random new one.
pub fn create_asset(
    sk_str: Option<&str>,
    memo: &str,
    decimal: u8,
    max_units: Option<u64>,
    transferable: bool,
    token_code: Option<&str>,
    is_address_eth: bool,
) -> Result<()> {
    let kp = restore_keypair_from_str_with_default(sk_str, is_address_eth)?;

    let code = if token_code.is_none() {
        AssetTypeCode::gen_random()
    } else {
        AssetTypeCode::new_from_base64(token_code.unwrap())
            .c(d!("invalid asset code"))?
    };

    create_asset_x(&kp, memo, decimal, max_units, transferable, Some(code))
        .c(d!())
        .map(|code| {
            println!("type: {}", code.to_base64());
        })
}

#[allow(missing_docs)]
pub fn create_asset_x(
    kp: &XfrKeyPair,
    memo: &str,
    decimal: u8,
    max_units: Option<u64>,
    transferable: bool,
    code: Option<AssetTypeCode>,
) -> Result<AssetTypeCode> {
    let code = code.unwrap_or_else(AssetTypeCode::gen_random);
    let asset_code = AssetTypeCode::from_prefix_and_raw_asset_type_code_2nd_update(
        AssetTypePrefix::UserDefined,
        &code,
    );

    let mut rules = AssetRules::default();
    rules.set_decimals(decimal).c(d!())?;
    rules.set_max_units(max_units);
    rules.set_transferable(transferable);

    let mut builder = utils::new_tx_builder().c(d!())?;
    builder
        .add_operation_create_asset(kp, Some(code), rules, memo)
        .c(d!())?;
    utils::gen_fee_op(kp)
        .c(d!())
        .map(|op| builder.add_operation(op))?;

    let mut tx = builder.build_and_take_transaction()?;
    tx.sign_to_map(kp);

    utils::send_tx(&tx).map(|_| asset_code)
}

/// Issue a custom asset with specified amount
pub fn issue_asset(
    sk_str: Option<&str>,
    asset: &str,
    amount: u64,
    hidden: bool,
    is_address_eth: bool,
) -> Result<()> {
    let kp = restore_keypair_from_str_with_default(sk_str, is_address_eth)?;
    let code = AssetTypeCode::new_from_base64(asset).c(d!())?;
    issue_asset_x(&kp, &code, amount, hidden).c(d!())
}

#[allow(missing_docs)]
pub fn issue_asset_x(
    kp: &XfrKeyPair,
    code: &AssetTypeCode,
    amount: u64,
    hidden: bool,
) -> Result<()> {
    let confidentiality_flags = AssetRecordType::from_flags(hidden, false);

    let mut builder = utils::new_tx_builder().c(d!())?;
    builder
        .add_basic_issue_asset(
            kp,
            code,
            builder.get_seq_id(),
            amount,
            confidentiality_flags,
        )
        .c(d!())?;
    utils::gen_fee_op(kp)
        .c(d!())
        .map(|op| builder.add_operation(op))?;

    let mut tx = builder.build_and_take_transaction()?;
    tx.sign_to_map(kp);

    utils::send_tx(&tx)
}

/// Show a list of custom asset token created by a findora account
pub fn show_asset(addr: &str) -> Result<()> {
    let pk = wallet::public_key_from_bech32(addr).c(d!())?;
    let assets = utils::get_created_assets(&pk).c(d!())?;
    for (code, _asset) in assets {
        let base64 = code.to_base64();
        let h = hex::encode(code.val.0);
        println!("Base64: {base64}, Hex: {h}");
    }
    Ok(())
}

/// Builds a transaction for a BAR to ABAR conversion with fees and sends it to network
/// # Arguments
/// * owner_sk - Optional secret key Xfr in json form
/// * target_addr - ABAR receiving AXfr pub key after conversion in base64
/// * TxoSID - sid of BAR to convert
pub fn convert_bar2abar(
    owner_sk: Option<&String>,
    target_addr: &str,
    txo_sid: &str,
    is_address_eth: bool,
) -> Result<Commitment> {
    // parse sender XfrSecretKey or generate from Mnemonic setup with wallet
    let from = match owner_sk {
        Some(str) => {
            ruc::info!(serde_json::from_str::<XfrSecretKey>(&format!("\"{str}\"",)))
                .c(d!())?
                .into_keypair()
        }
        None => get_keypair(is_address_eth).c(d!())?,
    };
    // parse receiver AxfrPubKey
    let to =
        wallet::public_key_from_bech32(target_addr).c(d!("invalid 'target-addr'"))?;
    let sid = txo_sid.parse::<u64>().c(d!("error parsing TxoSID"))?;

    // Get OpenAssetRecord from given Owner XfrKeyPair and TxoSID
    let record =
        utils::get_oar(&from, TxoSID(sid)).c(d!("error fetching open asset record"))?;
    let is_bar_transparent =
        record.1.get_record_type() == NonConfidentialAmount_NonConfidentialAssetType;

    // Generate the transaction and transmit it to network
    let c = utils::generate_bar2abar_op(
        &from,
        &to,
        TxoSID(sid),
        &record.0,
        is_bar_transparent,
    )
    .c(d!("Bar to abar failed"))?;

    Ok(c)
}

/// Convert an ABAR to a Blind Asset Record
/// # Arguments
/// * axfr_secret_key - the anon_secret_key in base64
/// * com             - commitment of ABAR in base64
/// * to              - Bar receiver's XfrPublicKey pointer
/// * com_fra         - commitment of the FRA ABAR to pay fee in base64
/// * confidential_am - if the output BAR should have confidential amount
/// * confidential_ty - if the output BAR should have confidential type
pub fn convert_abar2bar(
    owner_sk: Option<String>,
    com: &str,
    to: &XfrPublicKey,
    confidential_am: bool,
    confidential_ty: bool,
    is_address_eth: bool,
) -> Result<()> {
    let from = match owner_sk {
        Some(str) => {
            ruc::info!(serde_json::from_str::<XfrSecretKey>(&format!("\"{str}\"")))
                .c(d!())?
                .into_keypair()
        }
        None => get_keypair(is_address_eth).c(d!())?,
    };
    // Get the owned ABAR from pub_key and commitment
    let com = wallet::commitment_from_base58(com).c(d!())?;
    let axtxo_abar = utils::get_owned_abar(&com).c(d!())?;

    // get OwnerMemo and Merkle Proof of ABAR
    let owner_memo = utils::get_abar_memo(&axtxo_abar.0).c(d!())?.unwrap();
    let mt_leaf_info = utils::get_abar_proof(&axtxo_abar.0).c(d!())?.unwrap();
    let mt_leaf_uid = mt_leaf_info.uid;

    // Open ABAR with OwnerMemo & attach merkle proof
    let oabar_in = OpenAnonAssetRecordBuilder::from_abar(
        &axtxo_abar.1,
        owner_memo,
        &from.into_noah(),
    )
    .unwrap()
    .mt_leaf_info(mt_leaf_info)
    .build()
    .unwrap();

    // check oabar is unspent. If already spent return error
    // create nullifier
    let n = nullify(
        &from.into_noah(),
        oabar_in.get_amount(),
        oabar_in.get_asset_type().as_scalar(),
        mt_leaf_uid,
    )
    .c(d!())?;
    let hash = wallet::nullifier_to_base58(&n.0);
    // check if hash is present in nullifier set
    let null_status = utils::check_nullifier_hash(&hash)
        .c(d!())?
        .ok_or(d!("The ABAR corresponding to this commitment is missing"))?;
    if null_status {
        return Err(eg!(
            "The ABAR corresponding to this commitment is already spent"
        ));
    }
    println!("Nullifier: {}", wallet::nullifier_to_base58(&n.0));

    // Create New AssetRecordType for new BAR
    let art = match (confidential_am, confidential_ty) {
        (true, true) => AssetRecordType::ConfidentialAmount_ConfidentialAssetType,
        (true, false) => AssetRecordType::ConfidentialAmount_NonConfidentialAssetType,
        (false, true) => AssetRecordType::NonConfidentialAmount_ConfidentialAssetType,
        _ => AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
    };

    // Build AbarToBar Transaction and submit
    utils::generate_abar2bar_op(&oabar_in, &from, to, art).c(d!())?;

    Ok(())
}

/// Generate OABAR and add anonymous transfer operation
/// # Arguments
/// * axfr_secret_key - AXfrKeyPair in base64 form
/// * com             - Commitment in base64 form
/// * com_fra         - Commitment for paying fee
/// * amount          - amount to transfer
/// * to_axfr_public_key - AXfrPublicKey in base64 form
pub fn gen_anon_transfer_op(
    owner_sk: Option<String>,
    com: &str,
    com_fra: Option<&str>,
    amount: &str,
    to_address: &str,
    is_address_eth: bool,
) -> Result<()> {
    // parse sender keys
    // parse sender XfrSecretKey or generate from Mnemonic setup with wallet
    let from = match owner_sk {
        Some(str) => {
            ruc::info!(serde_json::from_str::<XfrSecretKey>(&format!("\"{str}\"")))
                .c(d!())?
                .into_keypair()
        }
        None => get_keypair(is_address_eth).c(d!())?,
    };
    let axfr_amount = amount.parse::<u64>().c(d!("error parsing amount"))?;

    let to = wallet::public_key_from_bech32(to_address)
        .c(d!("invalid 'to-xfr-public-key'"))?;

    let mut commitments = vec![com];
    if let Some(fra) = com_fra {
        commitments.push(fra);
    }
    let mut inputs = vec![];
    // For each commitment add input to transfer operation
    for com in commitments {
        let c = wallet::commitment_from_base58(com).c(d!())?;

        // get unspent ABARs & their Merkle proof for commitment
        let axtxo_abar = utils::get_owned_abar(&c).c(d!())?;
        let owner_memo = utils::get_abar_memo(&axtxo_abar.0).c(d!())?.unwrap();
        let mt_leaf_info = utils::get_abar_proof(&axtxo_abar.0).c(d!())?.unwrap();
        let mt_leaf_uid = mt_leaf_info.uid;

        // Create Open ABAR from input information
        let oabar_in = OpenAnonAssetRecordBuilder::from_abar(
            &axtxo_abar.1,
            owner_memo,
            &from.into_noah(),
        )
        .unwrap()
        .mt_leaf_info(mt_leaf_info)
        .build()
        .unwrap();

        // check oabar is unspent.
        let n = nullify(
            &from.into_noah(),
            oabar_in.get_amount(),
            oabar_in.get_asset_type().as_scalar(),
            mt_leaf_uid,
        )
        .c(d!())?;
        let hash = wallet::nullifier_to_base58(&n.0);
        let null_status = utils::check_nullifier_hash(&hash).c(d!())?.ok_or(d!(
            "The ABAR corresponding to this commitment is missing {}",
            com
        ))?;
        if null_status {
            return Err(eg!(
                "The ABAR corresponding to this commitment is already spent {}",
                com
            ));
        }

        println!("Nullifier: {}", wallet::nullifier_to_base58(&n.0));
        inputs.push(oabar_in);
    }

    // build output
    let mut prng = ChaChaRng::from_entropy();
    let oabar_out = OpenAnonAssetRecordBuilder::new()
        .amount(axfr_amount)
        .asset_type(inputs[0].get_asset_type())
        .pub_key(&to.into_noah())
        .finalize(&mut prng)
        .unwrap()
        .build()
        .unwrap();

    let mut builder: TransactionBuilder = new_tx_builder().c(d!())?;
    let (_, note, rem_oabars) = builder
        .add_operation_anon_transfer_fees_remainder(&inputs, &[oabar_out], &from)
        .c(d!())?;

    send_tx(&builder.build_and_take_transaction()?).c(d!())?;

    let com_out = if !note.body.outputs.is_empty() {
        Some(note.body.outputs[0].commitment)
    } else {
        None
    };

    if let Some(com) = com_out {
        println!(
            "\x1b[31;01m Commitment: {}\x1b[00m",
            wallet::commitment_to_base58(&com)
        );

        // Append receiver's commitment to `sent_commitment` file
        let mut file = fs::OpenOptions::new()
            .append(true)
            .create(true)
            .open("sent_commitments")
            .expect("cannot open commitments file");
        std::io::Write::write_all(
            &mut file,
            ("\n".to_owned() + &wallet::commitment_to_base58(&com)).as_bytes(),
        )
        .expect("commitment write failed");
    }

    // Append sender's fee balance commitment to `owned_commitments` file
    let mut file = fs::OpenOptions::new()
        .append(true)
        .create(true)
        .open("owned_commitments")
        .expect("cannot open commitments file");
    for rem_oabar in rem_oabars.iter() {
        let c = get_abar_commitment(rem_oabar.clone());
        println!(
            "\x1b[31;01m Remainder Commitment: {}\x1b[00m",
            wallet::commitment_to_base58(&c)
        );

        std::io::Write::write_all(
            &mut file,
            ("\n".to_owned() + &wallet::commitment_to_base58(&c)).as_bytes(),
        )
        .expect("commitment write failed");
    }

    println!("AxfrNote: {:?}", serde_json::to_string_pretty(&note.body));
    Ok(())
}

/// Batch anon transfer - Generate OABAR and add anonymous transfer operation
/// Note - if multiple anon keys are used, we consider the last key in the list for remainder.
/// # Arguments
/// * axfr_secret_key    - list of secret keys for senders' ABAR UTXOs
/// * to_axfr_public_keys - receiver AXfr Public keys
/// * to_enc_keys         - List of receiver Encryption keys
/// * commitments         - List of sender commitments in base64 format
/// * amounts             - List of receiver amounts
/// * assets              - List of receiver Asset Types
/// returns an error if Operation build fails
pub fn gen_oabar_add_op_x(
    owner_sk: Option<String>,
    to_axfr_public_keys: Vec<XfrPublicKey>,
    commitments: Vec<String>,
    amounts: Vec<String>,
    assets: Vec<AssetTypeCode>,
    is_address_eth: bool,
) -> Result<()> {
    let from = match owner_sk {
        Some(str) => {
            ruc::info!(serde_json::from_str::<XfrSecretKey>(&format!("\"{str}\"")))
                .c(d!())?
                .into_keypair()
        }
        None => get_keypair(is_address_eth).c(d!())?,
    };
    let receiver_count = to_axfr_public_keys.len();

    // check if input counts tally
    if receiver_count != amounts.len() || receiver_count != assets.len() {
        return Err(eg!(
            "The Parameters: from-sk/dec-keys/commitments or to-pk/to-enc-keys not match!"
        ));
    }

    // Create Input Open Abars with input keys, radomizers and Owner memos
    let mut oabars_in = Vec::new();
    for comm in commitments {
        let c = wallet::commitment_from_base58(comm.as_str()).c(d!())?;

        // Get OwnerMemo
        let axtxo_abar = utils::get_owned_abar(&c).c(d!())?;
        let owner_memo = utils::get_abar_memo(&axtxo_abar.0).c(d!(comm))?.unwrap();
        // Get Merkle Proof
        let mt_leaf_info = utils::get_abar_proof(&axtxo_abar.0).c(d!())?.unwrap();
        let mt_leaf_uid = mt_leaf_info.uid;

        // Build Abar
        let oabar_in = OpenAnonAssetRecordBuilder::from_abar(
            &axtxo_abar.1,
            owner_memo,
            &from.into_noah(),
        )
        .unwrap()
        .mt_leaf_info(mt_leaf_info)
        .build()
        .unwrap();

        // check oabar is unspent.
        let n = nullify(
            &from.into_noah(),
            oabar_in.get_amount(),
            oabar_in.get_asset_type().as_scalar(),
            mt_leaf_uid,
        )
        .c(d!())?;
        let hash = wallet::nullifier_to_base58(&n.0);
        let null_status = utils::check_nullifier_hash(&hash)
            .c(d!())?
            .ok_or(d!("The ABAR corresponding to this commitment is missing"))?;
        if null_status {
            return Err(eg!(
                "The ABAR corresponding to this commitment is already spent"
            ));
        }
        println!("Nullifier: {}", wallet::nullifier_to_base58(&n.0));

        oabars_in.push(oabar_in);
    }

    // Create output Open ABARs
    let mut oabars_out = Vec::new();
    for i in 0..receiver_count {
        let mut prng = ChaChaRng::from_entropy();
        let to = to_axfr_public_keys[i];
        let axfr_amount = amounts[i].parse::<u64>().c(d!("error parsing amount"))?;
        let asset_type = assets[i];

        let oabar_out = OpenAnonAssetRecordBuilder::new()
            .amount(axfr_amount)
            .asset_type(asset_type.val)
            .pub_key(&to.into_noah())
            .finalize(&mut prng)
            .unwrap()
            .build()
            .unwrap();

        oabars_out.push(oabar_out);
    }

    // Add a output for fees balance
    let mut builder: TransactionBuilder = new_tx_builder().c(d!())?;
    let (_, note, rem_oabars) = builder
        .add_operation_anon_transfer_fees_remainder(
            &oabars_in[..],
            &oabars_out[..],
            &from,
        )
        .c(d!())?;

    // Send the transaction to the network
    send_tx(&builder.build_and_take_transaction()?).c(d!())?;

    // Append receiver's commitment to `sent_commitments` file
    let mut s_file = fs::OpenOptions::new()
        .append(true)
        .create(true)
        .open("sent_commitments")
        .expect("cannot open commitments file");
    for oabar_out in oabars_out {
        let c_out = get_abar_commitment(oabar_out);
        println!(
            "\x1b[31;01m Commitment: {}\x1b[00m",
            wallet::commitment_to_base58(&c_out)
        );

        std::io::Write::write_all(
            &mut s_file,
            ("\n".to_owned() + &wallet::commitment_to_base58(&c_out)).as_bytes(),
        )
        .expect("commitment write failed");
    }

    let mut o_file = fs::OpenOptions::new()
        .append(true)
        .create(true)
        .open("owned_commitments")
        .expect("cannot open commitments file");
    for rem_oabar in rem_oabars.iter() {
        let c_rem = get_abar_commitment(rem_oabar.clone());

        println!(
            "\x1b[31;01m Remainder Commitment: {}\x1b[00m",
            wallet::commitment_to_base58(&c_rem)
        );
        std::io::Write::write_all(
            &mut o_file,
            ("\n".to_owned() + &wallet::commitment_to_base58(&c_rem)).as_bytes(),
        )
        .expect("commitment write failed");
    }

    println!("AxfrNote: {:?}", serde_json::to_string_pretty(&note.body));
    Ok(())
}

/// Get merkle proof - Generate MTLeafInfo from ATxoSID
pub fn get_mtleaf_info(atxo_sid: &str) -> Result<MTLeafInfo> {
    let asid = atxo_sid.parse::<u64>().c(d!("error parsing ATxoSID"))?;
    let mt_leaf_info = utils::get_abar_proof(&ATxoSID(asid))
        .c(d!("error fetching abar proof"))?
        .unwrap();
    Ok(mt_leaf_info)
}

/// Fetches list of owned TxoSIDs from LedgerStatus
pub fn get_owned_utxos(
    asset: Option<&str>,
    is_address_eth: bool,
) -> Result<Vec<(TxoSID, XfrAmount, XfrAssetType)>> {
    // get KeyPair from current setup wallet
    let kp = get_keypair(is_address_eth).c(d!())?;

    // Parse Asset Type for filtering if provided
    let mut asset_type = ASSET_TYPE_FRA;
    if let Some(a) = asset {
        asset_type = if a.to_uppercase() == "FRA" {
            ASSET_TYPE_FRA
        } else {
            AssetTypeCode::new_from_base64(asset.unwrap()).unwrap().val
        };
    }

    let list: Vec<(TxoSID, XfrAmount, XfrAssetType)> =
        utils::get_owned_utxos(&kp.pub_key)?
            .iter()
            .filter(|a| {
                // Filter by asset type if given or read all
                if asset.is_none() {
                    true
                } else {
                    match a.1.clone().0 .0.record.asset_type {
                        XfrAssetType::Confidential(_) => false,
                        XfrAssetType::NonConfidential(x) => asset_type == x,
                    }
                }
            })
            .map(|a| {
                let record = a.1.clone().0 .0.record;
                (*a.0, record.amount, record.asset_type)
            })
            .collect();

    Ok(list)
}

/// Check the spending status of an ABAR from AnonKeys and commitment
pub fn check_abar_status(
    from: XfrKeyPair,
    axtxo_abar: (ATxoSID, AnonAssetRecord),
) -> Result<()> {
    let owner_memo = utils::get_abar_memo(&axtxo_abar.0).c(d!())?.unwrap();
    let mt_leaf_info = utils::get_abar_proof(&axtxo_abar.0).c(d!())?.unwrap();
    let mt_leaf_uid = mt_leaf_info.uid;

    let oabar = OpenAnonAssetRecordBuilder::from_abar(
        &axtxo_abar.1,
        owner_memo,
        &from.into_noah(),
    )
    .unwrap()
    .mt_leaf_info(mt_leaf_info)
    .build()
    .unwrap();

    let n = nullify(
        &from.into_noah(),
        oabar.get_amount(),
        oabar.get_asset_type().as_scalar(),
        mt_leaf_uid,
    )
    .c(d!())?;
    let hash = wallet::nullifier_to_base58(&n.0);
    let null_status = utils::check_nullifier_hash(&hash).c(d!())?.unwrap();
    if null_status {
        println!("The ABAR corresponding to this commitment is already spent");
    } else {
        println!("The ABAR corresponding to this commitment is unspent and has a balance {:?}", oabar.get_amount());
    }
    Ok(())
}

/// Prints a dainty list of Abar info with spent status for a given AxfrKeyPair and a list of
/// commitments.
pub fn get_owned_abars(
    axfr_secret_key: XfrKeyPair,
    commitments_list: &str,
) -> Result<()> {
    println!("Abar data for commitments: {commitments_list}",);
    println!();
    println!(
        "{0: <8} | {1: <18} | {2: <45} | {3: <9} | {4: <45}",
        "ATxoSID", "Amount", "AssetType", "IsSpent", "Commitment"
    );
    println!("{:-^1$}", "", 184);
    commitments_list
        .split(',')
        .try_for_each(|com| -> ruc::Result<()> {
            let commitment = wallet::commitment_from_base58(com).c(d!())?;
            let (sid, abar) = utils::get_owned_abar(&commitment).c(d!())?;
            let memo = utils::get_abar_memo(&sid).unwrap().unwrap();
            let oabar = OpenAnonAssetRecordBuilder::from_abar(
                &abar,
                memo,
                &axfr_secret_key.into_noah(),
            )
            .unwrap()
            .build()
            .unwrap();

            let n = nullify(
                &axfr_secret_key.into_noah(),
                oabar.get_amount(),
                oabar.get_asset_type().as_scalar(),
                sid.0,
            )
            .c(d!())?;
            let hash = wallet::nullifier_to_base58(&n.0);
            let null_status = utils::check_nullifier_hash(&hash).c(d!())?.unwrap();
            println!(
                "{0: <8} | {1: <18} | {2: <45} | {3: <9} | {4: <45}",
                sid.0,
                oabar.get_amount(),
                AssetTypeCode {
                    val: oabar.get_asset_type()
                }
                .to_base64(),
                null_status,
                com
            );

            Ok(())
        })?;

    Ok(())
}

/// Prints a dainty list of Abar info with spent status for a given AxfrKeyPair and a list of
/// commitments.
pub fn anon_balance(
    axfr_secret_key: XfrKeyPair,
    commitments_list: &str,
    asset: Option<&str>,
) -> Result<()> {
    // Parse Asset Type for filtering if provided
    let mut asset_type = ASSET_TYPE_FRA;
    if let Some(a) = asset {
        asset_type = if a.to_uppercase() == "FRA" {
            ASSET_TYPE_FRA
        } else {
            AssetTypeCode::new_from_base64(asset.unwrap()).unwrap().val
        };
    }

    let mut balance = 0u64;
    commitments_list
        .split(',')
        .try_for_each(|com| -> ruc::Result<()> {
            let commitment = wallet::commitment_from_base58(com).c(d!())?;

            let result = utils::get_owned_abar(&commitment);
            match result {
                Err(e) => {
                    if e.msg_eq(eg!("missing abar").as_ref()) {
                        Ok(())
                    } else {
                        Err(e)
                    }
                }
                Ok((sid, abar)) => {
                    let memo = utils::get_abar_memo(&sid).unwrap().unwrap();
                    let oabar = OpenAnonAssetRecordBuilder::from_abar(
                        &abar,
                        memo,
                        &axfr_secret_key.into_noah(),
                    )
                    .unwrap()
                    .build()
                    .unwrap();

                    let n = nullify(
                        &axfr_secret_key.into_noah(),
                        oabar.get_amount(),
                        oabar.get_asset_type().as_scalar(),
                        sid.0,
                    )
                    .c(d!())?;
                    let hash = wallet::nullifier_to_base58(&n.0);
                    let is_spent = utils::check_nullifier_hash(&hash).c(d!())?.unwrap();
                    if !is_spent && oabar.get_asset_type() == asset_type {
                        balance += oabar.get_amount();
                    }

                    Ok(())
                }
            }
        })?;

    println!("{}: {}", asset.unwrap_or("FRA"), balance);
    Ok(())
}

/// Return the built version.
pub fn version() -> &'static str {
    concat!(env!("VERGEN_SHA"), " ", env!("VERGEN_BUILD_DATE"))
}

///operation to replace the staker.
pub fn replace_staker(
    target_addr: fp_types::H160,
    td_addr: &str,
    is_address_eth: bool,
) -> Result<()> {
    let td_addr = hex::decode(td_addr).c(d!())?;
    let keypair = get_keypair(is_address_eth)?;

    let mut builder = utils::new_tx_builder().c(d!())?;

    utils::gen_fee_op(&keypair).c(d!()).map(|op| {
        builder.add_operation(op);
    })?;

    builder.add_operation_replace_staker(&keypair, target_addr, None, td_addr)?;
    let mut tx = builder.take_transaction();
    tx.sign_to_map(&keypair);

    utils::send_tx(&tx).c(d!())?;
    Ok(())
}
