//!
//! # Findora Network Cli tool
//!
//! FN, a command line tool for findora network.
//!
//! This module is the library part of FN.
//!

#[cfg(not(target_arch = "wasm32"))]
pub mod dev;

#[cfg(not(target_arch = "wasm32"))]
pub mod ddev;

pub mod evm;
pub mod utils;

use {
    crate::api::DelegationInfo,
    globutils::wallet,
    lazy_static::lazy_static,
    ledger::{
        data_model::{
            gen_random_keypair, AssetRules, AssetTypeCode, Transaction,
            BLACK_HOLE_PUBKEY_STAKING,
        },
        staking::{
            check_delegation_amount, td_addr_to_bytes, td_pubkey_to_td_addr,
            td_pubkey_to_td_addr_bytes, PartialUnDelegation, StakerMemo,
            TendermintAddrRef,
        },
    },
    ruc::*,
    std::{env, fs},
    tendermint::PrivateKey,
    utils::{
        get_block_height, get_local_block_height, get_validator_detail,
        parse_td_validator_keys,
    },
    zei::{
        setup::PublicParams,
        xfr::{
            asset_record::AssetRecordType,
            sig::{XfrKeyPair, XfrPublicKey, XfrSecretKey},
        },
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
pub fn staker_update(cr: Option<&str>, memo: Option<StakerMemo>) -> Result<()> {
    let addr = get_td_pubkey().map(|i| td_pubkey_to_td_addr(&i)).c(d!())?;
    let vd = get_validator_detail(&addr).c(d!())?;

    let cr = cr
        .map_or(Ok(vd.commission_rate), |s| {
            s.parse::<f64>()
                .c(d!("commission rate must be a float number"))
                .and_then(convert_commission_rate)
        })
        .c(d!())?;
    let memo = memo.unwrap_or(vd.memo);

    let td_pubkey = get_td_pubkey().c(d!())?;

    let kp = get_keypair().c(d!())?;
    let vkp = get_td_privkey().c(d!())?;

    let mut builder = utils::new_tx_builder().c(d!())?;

    builder
        .add_operation_update_staker(&kp, &vkp, td_pubkey, cr, memo)
        .c(d!())?;
    utils::gen_fee_op(&kp)
        .c(d!())
        .map(|op| builder.add_operation(op))?;

    let mut tx = builder.take_transaction();
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
) -> Result<()> {
    let am = amount.parse::<u64>().c(d!("'amount' must be an integer"))?;
    check_delegation_amount(am, false).c(d!())?;
    let cr = commission_rate
        .parse::<f64>()
        .c(d!("commission rate must be a float number"))
        .and_then(|cr| convert_commission_rate(cr).c(d!()))?;
    let td_pubkey = get_td_pubkey().c(d!())?;

    let kp = get_keypair().c(d!())?;
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
            "The difference in block height of your node and the remote network is too big: \n remote / local: {} / {}",
            network_height, local_height
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
        vec![(&BLACK_HOLE_PUBKEY_STAKING, am)],
        None,
        false,
        false,
        Some(AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType),
    )
    .c(d!())
    .map(|principal_op| builder.add_operation(principal_op))?;

    let mut tx = builder.take_transaction();
    tx.sign_to_map(&kp);

    utils::send_tx(&tx).c(d!())
}

/// Append more FRA token to the specified tendermint node
pub fn stake_append(
    amount: &str,
    staker: Option<&str>,
    td_addr: Option<TendermintAddrRef>,
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
        .or_else(|_| get_keypair().c(d!()))?;

    let mut builder = utils::new_tx_builder().c(d!())?;
    builder.add_operation_delegation(&kp, am, td_addr);
    utils::gen_transfer_op(
        &kp,
        vec![(&BLACK_HOLE_PUBKEY_STAKING, am)],
        None,
        false,
        false,
        Some(AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType),
    )
    .c(d!())
    .map(|principal_op| builder.add_operation(principal_op))?;
    let mut tx = builder.take_transaction();
    tx.sign_to_map(&kp);

    utils::send_tx(&tx).c(d!())
}

/// Withdraw Fra token from findora network for a staker
pub fn unstake(
    am: Option<&str>,
    staker: Option<&str>,
    td_addr: Option<TendermintAddrRef>,
) -> Result<()> {
    let am = if let Some(i) = am {
        Some(i.parse::<u64>().c(d!("'amount' must be an integer"))?)
    } else {
        None
    };

    let kp = staker
        .c(d!())
        .and_then(|sk| wallet::restore_keypair_from_mnemonic_default(sk).c(d!()))
        .or_else(|_| get_keypair().c(d!()))?;
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

    let mut tx = builder.take_transaction();
    tx.sign_to_map(&kp);

    utils::send_tx(&tx).c(d!())
}

/// Claim rewards from findora network
pub fn claim(am: Option<&str>, sk_str: Option<&str>) -> Result<()> {
    let am = if let Some(i) = am {
        Some(i.parse::<u64>().c(d!("'amount' must be an integer"))?)
    } else {
        None
    };

    let kp = restore_keypair_from_str_with_default(sk_str)?;

    let mut builder = utils::new_tx_builder().c(d!())?;

    utils::gen_fee_op(&kp).c(d!()).map(|op| {
        builder.add_operation(op);
        builder.add_operation_claim(&kp, am);
    })?;

    let mut tx = builder.take_transaction();
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
pub fn show(basic: bool) -> Result<()> {
    let kp = get_keypair().c(d!())?;

    let serv_addr = ruc::info!(get_serv_addr()).map(|i| {
        println!("\x1b[31;01mServer URL:\x1b[00m\n{}\n", i);
    });

    let xfr_account = ruc::info!(get_keypair()).map(|i| {
        println!(
            "\x1b[31;01mFindora Address:\x1b[00m\n{}\n",
            wallet::public_key_to_bech32(&i.get_pk())
        );
        println!(
            "\x1b[31;01mFindora Public Key:\x1b[00m\n{}\n",
            wallet::public_key_to_base64(&i.get_pk())
        );
    });

    let self_balance = ruc::info!(utils::get_balance(&kp)).map(|i| {
        println!("\x1b[31;01mNode Balance:\x1b[00m\n{} FRA units\n", i);
    });

    if basic {
        return Ok(());
    }

    let td_info = ruc::info!(get_td_pubkey()).map(|i| {
        let addr = td_pubkey_to_td_addr(&i);
        println!("\x1b[31;01mValidator Node Addr:\x1b[00m\n{}\n", addr);
        (i, addr)
    });

    let di = utils::get_delegation_info(kp.get_pk_ref());
    let bond_entries = match di.as_ref() {
        Ok(di) => Some(di.bond_entries.clone()),
        Err(_) => None,
    };

    let delegation_info = di.and_then(|di| {
        serde_json::to_string_pretty(&di).c(d!("server returned invalid data"))
    });
    let delegation_info = ruc::info!(delegation_info).map(|i| {
        println!("\x1b[31;01mYour Delegation:\x1b[00m\n{}\n", i);
    });

    if let Ok((tpk, addr)) = td_info.as_ref() {
        let self_delegation =
            bond_entries.map_or(false, |bes| bes.iter().any(|i| &i.0 == addr));
        if self_delegation {
            let res = utils::get_validator_detail(&td_pubkey_to_td_addr(tpk))
                .c(d!("Validator not found"))
                .and_then(|di| {
                    serde_json::to_string_pretty(&di)
                        .c(d!("server returned invalid data"))
                })
                .map(|i| {
                    println!("\x1b[31;01mYour Staking:\x1b[00m\n{}\n", i);
                });
            ruc::info_omit!(res);
        }
    }

    if [
        serv_addr,
        xfr_account,
        td_info.map(|_| ()),
        self_balance,
        delegation_info,
    ]
    .iter()
    .any(|i| i.is_err())
    {
        Err(eg!("unable to obtain complete information"))
    } else {
        Ok(())
    }
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
) -> Result<()> {
    transfer_asset_batch(
        owner_sk,
        &[target_addr],
        token_code,
        am,
        confidential_am,
        confidential_ty,
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
) -> Result<()> {
    let from = restore_keypair_from_str_with_default(owner_sk)?;
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
        target_addr.iter().map(|addr| (addr, am)).collect(),
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
pub fn get_keypair() -> Result<XfrKeyPair> {
    if let Some(m_path) = MNEMONIC.as_ref() {
        fs::read_to_string(m_path)
            .c(d!("can not read mnemonic from 'owner-mnemonic-path'"))
            .and_then(|m| {
                let k = m.trim();
                wallet::restore_keypair_from_mnemonic_default(k)
                    .c(d!("invalid 'owner-mnemonic'"))
                    .or_else(|e| wallet::restore_keypair_from_seckey_base64(k).c(d!(e)))
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
pub fn gen_key() -> (String, String, String, XfrKeyPair) {
    let (mnemonic, key, kp) = loop {
        let mnemonic = pnk!(wallet::generate_mnemonic_custom(24, "en"));
        let kp = pnk!(wallet::restore_keypair_from_mnemonic_default(&mnemonic));
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
pub fn gen_key_and_print() {
    let (wallet_addr, mnemonic, key, _) = gen_key();
    println!(
        "\n\x1b[31;01mWallet Address:\x1b[00m {}\n\x1b[31;01mMnemonic:\x1b[00m {}\n\x1b[31;01mKey:\x1b[00m {}\n",
        wallet_addr, mnemonic, key
    );
}

fn restore_keypair_from_str_with_default(sk_str: Option<&str>) -> Result<XfrKeyPair> {
    if let Some(sk) = sk_str {
        serde_json::from_str::<XfrSecretKey>(&format!("\"{}\"", sk.trim()))
            .map(|sk| sk.into_keypair())
            .c(d!("Invalid secret key"))
    } else {
        get_keypair().c(d!())
    }
}

/// Show the asset balance of a findora account
pub fn show_account(sk_str: Option<&str>, asset: Option<&str>) -> Result<()> {
    let kp = restore_keypair_from_str_with_default(sk_str)?;
    let token_code = asset
        .map(|asset| AssetTypeCode::new_from_base64(asset).c(d!("Invalid asset code")))
        .transpose()?;
    let balance = utils::get_asset_balance(&kp, token_code).c(d!())?;

    println!("{}: {}", asset.unwrap_or("FRA"), balance);
    Ok(())
}

#[inline(always)]
#[allow(missing_docs)]
pub fn delegate(sk_str: Option<&str>, amount: u64, validator: &str) -> Result<()> {
    restore_keypair_from_str_with_default(sk_str)
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
pub fn undelegate(sk_str: Option<&str>, param: Option<(u64, &str)>) -> Result<()> {
    restore_keypair_from_str_with_default(sk_str)
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
pub fn show_delegations(sk_str: Option<&str>) -> Result<()> {
    let pk = restore_keypair_from_str_with_default(sk_str)?.get_pk();

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

    let mut tx = builder.take_transaction();
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
        vec![(&BLACK_HOLE_PUBKEY_STAKING, amount)],
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

    let mut tx = builder.take_transaction();

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
) -> Result<()> {
    let kp = restore_keypair_from_str_with_default(sk_str)?;

    let code = if token_code.is_none() {
        AssetTypeCode::gen_random()
    } else {
        AssetTypeCode::new_from_base64(token_code.unwrap())
            .c(d!("invalid asset code"))?
    };

    create_asset_x(&kp, memo, decimal, max_units, transferable, Some(code))
        .c(d!())
        .map(|_| ())
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

    let mut tx = builder.take_transaction();
    tx.sign_to_map(kp);

    utils::send_tx(&tx).map(|_| code)
}

/// Issue a custom asset with specified amount
pub fn issue_asset(
    sk_str: Option<&str>,
    asset: &str,
    amount: u64,
    hidden: bool,
) -> Result<()> {
    let kp = restore_keypair_from_str_with_default(sk_str)?;
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
            &PublicParams::default(),
        )
        .c(d!())?;
    utils::gen_fee_op(kp)
        .c(d!())
        .map(|op| builder.add_operation(op))?;

    let mut tx = builder.take_transaction();
    tx.sign_to_map(kp);

    utils::send_tx(&tx)
}

/// Show a list of custom asset token created by a findora account
pub fn show_asset(addr: &str) -> Result<()> {
    let pk = wallet::public_key_from_bech32(addr).c(d!())?;
    let assets = utils::get_created_assets(&pk).c(d!())?;
    assets
        .iter()
        .for_each(|asset| println!("{}", asset.body.asset.code.to_base64()));
    Ok(())
}

/// Return the built version.
pub fn version() -> &'static str {
    concat!(env!("VERGEN_SHA"), " ", env!("VERGEN_BUILD_DATE"))
}

///operation to replace the staker.
pub fn replace_staker(
    target_pubkey: XfrPublicKey,
    new_td_addr_pk: Option<(Vec<u8>, Vec<u8>)>,
) -> Result<()> {
    let keypair = get_keypair()?;

    let mut builder = utils::new_tx_builder().c(d!())?;

    utils::gen_fee_op(&keypair).c(d!()).map(|op| {
        builder.add_operation(op);
    })?;

    builder.add_operation_replace_staker(&keypair, target_pubkey, new_td_addr_pk)?;
    let mut tx = builder.take_transaction();
    tx.sign_to_map(&keypair);

    utils::send_tx(&tx).c(d!())?;
    Ok(())
}
