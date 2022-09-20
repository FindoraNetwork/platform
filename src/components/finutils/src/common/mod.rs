//!
//! # Findora Network Cli tool
//!
//! FN, a command line tool for findora network.
//!
//! This module is the library part of FN.
//!

pub mod evm;
pub mod utils;

use {
    crate::api::DelegationInfo,
    crate::common::utils::{new_tx_builder, send_tx},
    crate::txn_builder::TransactionBuilder,
    fp_utils::hashing::keccak_256,
    globutils::wallet,
    lazy_static::lazy_static,
    ledger::{
        data_model::{
            gen_random_keypair, ATxoSID, AssetRules, AssetTypeCode, AssetTypePrefix,
            Transaction, TxoSID, ASSET_TYPE_FRA, BLACK_HOLE_PUBKEY_STAKING,
        },
        staking::{
            check_delegation_amount, td_addr_to_bytes, td_pubkey_to_td_addr,
            td_pubkey_to_td_addr_bytes, PartialUnDelegation, StakerMemo,
            TendermintAddrRef,
        },
        store::fbnc::NumKey,
    },
    rand_chacha::ChaChaRng,
    rand_core::SeedableRng,
    ruc::*,
    std::{collections::HashMap, env, fs},
    tendermint::PrivateKey,
    utils::{
        get_block_height, get_local_block_height, get_validator_detail,
        parse_td_validator_keys,
    },
    zei::{
        anon_xfr::{
            keys::{AXfrKeyPair, AXfrPubKey},
            nullify_with_native_address,
            structs::{
                AnonAssetRecord, Commitment, MTLeafInfo, OpenAnonAssetRecordBuilder,
            },
        },
        xfr::{
            asset_record::{
                AssetRecordType,
                AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType,
            },
            sig::{XfrKeyPair, XfrPublicKey, XfrSecretKey},
            structs::{AssetType, XfrAmount, XfrAssetType},
        },
    },
    zei_algebra::utils::b64enc,
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

    let mut tx = builder.build_and_take_transaction()?;
    tx.sign(&kp);

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

    let mut tx = builder.build_and_take_transaction()?;
    tx.sign(&kp);

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

    utils::send_tx(&builder.build_and_take_transaction()?).c(d!())
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

    let mut tx = builder.build_and_take_transaction()?;
    tx.sign(&kp);

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

    let mut tx = builder.build_and_take_transaction()?;
    tx.sign(&kp);

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
        println!(
            "\x1b[31;01mFindora Public Key in hex:\x1b[00m\n{}\n",
            wallet::public_key_to_hex(&i.get_pk())
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
pub fn set_initial_validators(staking_info_file: Option<&str>) -> Result<()> {
    utils::set_initial_validators(staking_info_file).c(d!())
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

///Get tendermint public key
pub(crate) fn get_td_pubkey() -> Result<Vec<u8>> {
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
pub fn gen_key_and_print() {
    let (m, k, kp) = loop {
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
    println!(
        "\n\x1b[31;01mWallet Address:\x1b[00m {}\n\x1b[31;01mMnemonic:\x1b[00m {}\n\x1b[31;01mKey:\x1b[00m {}\n",
        wallet_addr, m, k
    );
}

fn restore_keypair_from_str_with_default(sk_str: Option<&str>) -> Result<XfrKeyPair> {
    if let Some(sk) = sk_str {
        serde_json::from_str::<XfrSecretKey>(&format!("\"{}\"", sk))
            .map(|sk| sk.into_keypair())
            .c(d!("Invalid secret key"))
    } else {
        get_keypair().c(d!())
    }
}

/// Show the asset balance of a findora account
pub fn show_account(sk_str: Option<&str>, asset: Option<AssetTypeCode>) -> Result<()> {
    let kp = restore_keypair_from_str_with_default(sk_str)?;

    println!("{0: <45} | {1: <70} | {2: <18} ", "Base64", "Hex", "Amount");
    let list = get_owned_utxos(Some(kp), asset)?;
    let mut map = HashMap::new();
    for (_, b, c) in list {
        let amt = b.get_amount();

        let at = c.get_asset_type().unwrap_or_default();
        let at_base64 = b64enc(&at.0);
        let at_hex = hex::encode(&at.0);

        let key = (at_base64, at_hex);

        if let Some(Some(a)) = map.get_mut(&key) {
            *a += amt.unwrap_or(0);
        } else {
            map.insert(key, amt);
        }
    }

    for (key, val) in map.iter() {
        let v = val.map_or_else(|| "Confidential".to_string(), |a| a.to_string());
        let hex = format!("0x{}", key.1);
        println!("{0: <45} | {1: <70} | {2: <18} ", key.0, hex, v);
    }

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

    let mut tx = builder.build_and_take_transaction()?;
    tx.sign(owner_kp);

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

    let mut tx = builder.build_and_take_transaction()?;
    tx.sign(owner_kp);

    Ok(tx)
}
/// Create a custom asset for a findora account. If no token code string provided,
/// it will generate a random new one.
pub fn create_asset(
    memo: &str,
    decimal: u8,
    max_units: Option<u64>,
    transferable: bool,
    token_code: Option<AssetTypeCode>,
) -> Result<()> {
    let kp = get_keypair().c(d!())?;

    create_asset_x(&kp, memo, decimal, max_units, transferable, token_code)
        .c(d!())
        .map(|code| {
            let hex = hex::encode(code.val.0);
            println!("typeBase64: {}, typeHex: {}", code.to_base64(), hex);
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

    let mut asset_code = AssetTypePrefix::UserDefined.bytes();
    asset_code.append(&mut code.to_bytes());

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
    tx.sign(kp);

    utils::send_tx(&tx).map(|_| AssetTypeCode {
        val: AssetType(keccak_256(&asset_code)),
    })
}

/// Issue a custom asset with specified amount
pub fn issue_asset(
    sk_str: Option<&str>,
    asset: Option<AssetTypeCode>,
    amount: u64,
    hidden: bool,
) -> Result<()> {
    let kp = restore_keypair_from_str_with_default(sk_str)?;
    let code = asset.c(d!())?;
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
    tx.sign(kp);

    utils::send_tx(&tx)
}

/// Show a list of custom asset token created by a findora account
pub fn show_asset(addr: &str) -> Result<()> {
    let pk = wallet::public_key_from_bech32(addr).c(d!())?;
    let assets = utils::get_created_assets(&pk).c(d!())?;
    for asset in assets {
        let base64 = asset.body.asset.code.to_base64();
        let h = hex::encode(asset.body.asset.code.val.0);
        println!("Base64: {}, Hex: {}", base64, h);
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
    target_addr: String,
    txo_sid: &str,
) -> Result<Commitment> {
    // parse sender XfrSecretKey or generate from Mnemonic setup with wallet
    let from = match owner_sk {
        Some(str) => ruc::info!(serde_json::from_str::<XfrSecretKey>(&format!(
            "\"{}\"",
            str
        )))
        .c(d!())?
        .into_keypair(),
        None => get_keypair().c(d!())?,
    };
    // parse receiver AxfrPubKey
    let to = wallet::anon_public_key_from_base64(target_addr.as_str())
        .c(d!("invalid 'target-addr'"))?;
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
    axfr_secret_key: String,
    com: &str,
    to: &XfrPublicKey,
    confidential_am: bool,
    confidential_ty: bool,
) -> Result<()> {
    // parse anon keys
    let from = wallet::anon_secret_key_from_base64(axfr_secret_key.as_str())
        .c(d!("invalid 'from-axfr-secret-key'"))?;

    // Get the owned ABAR from pub_key and commitment
    let com = wallet::commitment_from_base58(com).c(d!())?;
    let axtxo_abar = utils::get_owned_abar(&com).c(d!())?;

    // get OwnerMemo and Merkle Proof of ABAR
    let owner_memo = utils::get_abar_memo(&axtxo_abar.0).c(d!())?.unwrap();
    let mt_leaf_info = utils::get_abar_proof(&axtxo_abar.0).c(d!())?.unwrap();
    let mt_leaf_uid = mt_leaf_info.uid;

    // Open ABAR with OwnerMemo & attach merkle proof
    let oabar_in =
        OpenAnonAssetRecordBuilder::from_abar(&axtxo_abar.1, owner_memo, &from)
            .unwrap()
            .mt_leaf_info(mt_leaf_info)
            .build()
            .unwrap();

    // check oabar is unspent. If already spent return error
    // create nullifier
    let n = nullify_with_native_address(
        &from,
        oabar_in.get_amount(),
        &oabar_in.get_asset_type(),
        mt_leaf_uid,
    );
    let hash = wallet::nullifier_to_base58(&n);
    // check if hash is present in nullifier set
    let null_status = utils::check_nullifier_hash(&hash)
        .c(d!())?
        .ok_or(d!("The ABAR corresponding to this commitment is missing"))?;
    if null_status {
        return Err(eg!(
            "The ABAR corresponding to this commitment is already spent"
        ));
    }
    println!("Nullifier: {}", wallet::nullifier_to_base58(&n));

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
    axfr_secret_key: String,
    com: &str,
    com_fra: Option<&str>,
    amount: &str,
    to_axfr_public_key: &str,
) -> Result<()> {
    // parse sender keys
    let from = wallet::anon_secret_key_from_base64(axfr_secret_key.as_str())
        .c(d!("invalid 'from-axfr-secret-key'"))?;

    let axfr_amount = amount.parse::<u64>().c(d!("error parsing amount"))?;

    let to = wallet::anon_public_key_from_base64(to_axfr_public_key)
        .c(d!("invalid 'to-axfr-public-key'"))?;

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
        let oabar_in =
            OpenAnonAssetRecordBuilder::from_abar(&axtxo_abar.1, owner_memo, &from)
                .unwrap()
                .mt_leaf_info(mt_leaf_info)
                .build()
                .unwrap();

        // check oabar is unspent.
        let n = nullify_with_native_address(
            &from,
            oabar_in.get_amount(),
            &oabar_in.get_asset_type(),
            mt_leaf_uid,
        );
        let hash = wallet::nullifier_to_base58(&n);
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

        println!("Nullifier: {}", wallet::nullifier_to_base58(&n));
        inputs.push(oabar_in);
    }

    let froms = vec![from; inputs.len()];

    // build output
    let mut prng = ChaChaRng::from_entropy();
    let oabar_out = OpenAnonAssetRecordBuilder::new()
        .amount(axfr_amount)
        .asset_type(inputs[0].get_asset_type())
        .pub_key(&to)
        .finalize(&mut prng)
        .unwrap()
        .build()
        .unwrap();

    let mut builder: TransactionBuilder = new_tx_builder().c(d!())?;
    let (_, note, rem_oabars) = builder
        .add_operation_anon_transfer_fees_remainder(&inputs, &[oabar_out], &froms)
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
        let c = rem_oabar.compute_commitment();
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
/// * axfr_secret_keys    - list of secret keys for senders' ABAR UTXOs
/// * to_axfr_public_keys - receiver AXfr Public keys
/// * to_enc_keys         - List of receiver Encryption keys
/// * commitments         - List of sender commitments in base64 format
/// * amounts             - List of receiver amounts
/// * assets              - List of receiver Asset Types
/// returns an error if Operation build fails
pub fn gen_oabar_add_op_x(
    axfr_secret_keys: Vec<AXfrKeyPair>,
    to_axfr_public_keys: Vec<AXfrPubKey>,
    commitments: Vec<String>,
    amounts: Vec<String>,
    assets: Vec<AssetTypeCode>,
) -> Result<()> {
    let sender_count = axfr_secret_keys.len();
    let receiver_count = to_axfr_public_keys.len();

    // check if input counts tally
    if sender_count != commitments.len()
        || receiver_count != amounts.len()
        || receiver_count != assets.len()
    {
        return Err(eg!(
            "The Parameters: from-sk/dec-keys/commitments or to-pk/to-enc-keys not match!"
        ));
    }

    // Create Input Open Abars with input keys, radomizers and Owner memos
    let mut oabars_in = Vec::new();
    for i in 0..sender_count {
        let from = &axfr_secret_keys[i];
        let c = wallet::commitment_from_base58(commitments[i].as_str()).c(d!())?;

        // Get OwnerMemo
        let axtxo_abar = utils::get_owned_abar(&c).c(d!())?;
        let owner_memo = utils::get_abar_memo(&axtxo_abar.0)
            .c(d!(commitments[i]))?
            .unwrap();
        // Get Merkle Proof
        let mt_leaf_info = utils::get_abar_proof(&axtxo_abar.0).c(d!())?.unwrap();
        let mt_leaf_uid = mt_leaf_info.uid;

        // Build Abar
        let oabar_in =
            OpenAnonAssetRecordBuilder::from_abar(&axtxo_abar.1, owner_memo, from)
                .unwrap()
                .mt_leaf_info(mt_leaf_info)
                .build()
                .unwrap();

        // check oabar is unspent.
        let n = nullify_with_native_address(
            from,
            oabar_in.get_amount(),
            &oabar_in.get_asset_type(),
            mt_leaf_uid,
        );
        let hash = wallet::nullifier_to_base58(&n);
        let null_status = utils::check_nullifier_hash(&hash)
            .c(d!())?
            .ok_or(d!("The ABAR corresponding to this commitment is missing"))?;
        if null_status {
            return Err(eg!(
                "The ABAR corresponding to this commitment is already spent"
            ));
        }
        println!("Nullifier: {}", wallet::nullifier_to_base58(&n));

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
            .pub_key(&to)
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
            &axfr_secret_keys,
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
        let c_out = oabar_out.compute_commitment();
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
        let c_rem = rem_oabar.compute_commitment();

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
    kp: Option<XfrKeyPair>,
    asset: Option<AssetTypeCode>,
) -> Result<Vec<(TxoSID, XfrAmount, XfrAssetType)>> {
    // get KeyPair from current setup wallet
    let kp = if let Some(kp) = kp {
        kp
    } else {
        get_keypair().c(d!())?
    };

    let (is_all, asset_type) = if let Some(a) = asset {
        (false, a.val)
    } else {
        (true, AssetType::default())
    };

    let list: Vec<(TxoSID, XfrAmount, XfrAssetType)> =
        utils::get_owned_utxos(&kp.pub_key)?
            .iter()
            .filter(|a| {
                // Filter by asset type if given or read all
                if is_all {
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
    from: AXfrKeyPair,
    axtxo_abar: (ATxoSID, AnonAssetRecord),
) -> Result<()> {
    let owner_memo = utils::get_abar_memo(&axtxo_abar.0).c(d!())?.unwrap();
    let mt_leaf_info = utils::get_abar_proof(&axtxo_abar.0).c(d!())?.unwrap();
    let mt_leaf_uid = mt_leaf_info.uid;

    let oabar = OpenAnonAssetRecordBuilder::from_abar(&axtxo_abar.1, owner_memo, &from)
        .unwrap()
        .mt_leaf_info(mt_leaf_info)
        .build()
        .unwrap();

    let n = nullify_with_native_address(
        &from,
        oabar.get_amount(),
        &oabar.get_asset_type(),
        mt_leaf_uid,
    );
    let hash = wallet::nullifier_to_base58(&n);
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
    axfr_secret_key: AXfrKeyPair,
    commitments_list: &str,
) -> Result<()> {
    println!("Abar data for commitments: {}", commitments_list);
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
            let oabar =
                OpenAnonAssetRecordBuilder::from_abar(&abar, memo, &axfr_secret_key)
                    .unwrap()
                    .build()
                    .unwrap();

            let n = nullify_with_native_address(
                &axfr_secret_key,
                oabar.get_amount(),
                &oabar.get_asset_type(),
                sid.0,
            );
            let hash = wallet::nullifier_to_base58(&n);
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
    axfr_secret_key: AXfrKeyPair,
    commitments_list: &str,
    asset: Option<AssetTypeCode>,
) -> Result<()> {
    // Parse Asset Type for filtering if provided
    let asset_type = asset.map(|code| code.val).unwrap_or(ASSET_TYPE_FRA);

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
                        &axfr_secret_key,
                    )
                    .unwrap()
                    .build()
                    .unwrap();

                    let n = nullify_with_native_address(
                        &axfr_secret_key,
                        oabar.get_amount(),
                        &oabar.get_asset_type(),
                        sid.0,
                    );
                    let hash = wallet::nullifier_to_base58(&n);
                    let is_spent = utils::check_nullifier_hash(&hash).c(d!())?.unwrap();
                    if !is_spent && oabar.get_asset_type() == asset_type {
                        balance += oabar.get_amount();
                    }

                    Ok(())
                }
            }
        })?;
    println!("{0: <45} | {1: <70} | {2: <18} ", "Base64", "Hex", "Amount");
    let at_base64 = b64enc(&asset_type.0);
    let at_hex = hex::encode(&asset_type.0);
    println!(
        "{0: <45} | {1: <70} | {2: <18} ",
        at_base64, at_hex, balance
    );
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
    let mut tx = builder.build_and_take_transaction()?;
    tx.sign(&keypair);

    utils::send_tx(&tx).c(d!())?;
    Ok(())
}
