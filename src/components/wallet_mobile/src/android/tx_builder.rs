use crate::rust::*;
use jni::objects::{JClass, JString};
use jni::sys::{jboolean, jint, jlong, jstring, JNI_TRUE};
use jni::JNIEnv;
use zei::xfr::sig::XfrKeyPair;

#[no_mangle]
/// # Safety
/// @param am: amount to pay
/// @param kp: owner's XfrKeyPair
pub unsafe extern "system" fn Java_com_findora_JniApi_transactionBuilderAddFeeRelativeAuto(
    _env: JNIEnv,
    _: JClass,
    builder: jlong,
    kp: jlong,
) -> jlong {
    let builder = &*(builder as *mut TransactionBuilder);
    let kp = &*(kp as *mut XfrKeyPair);
    let builder = builder.clone().add_fee_relative_auto(kp.clone()).unwrap();
    Box::into_raw(Box::new(builder)) as jlong
}

// /// Use this func to get the necessary infomations for generating `Relative Inputs`
// ///
// /// - TxoRef::Relative("Element index of the result")
// /// - ClientAssetRecord::from_json("Element of the result")
// #[no_mangle]
// pub unsafe extern "system" fn Java_com_findora_JniApi_transactionBuilderGetRelativeOutputs(
//     env: JNIEnv,
//     _: JClass,
//     builder: jlong,
// ) ->  Vec<ClientAssetRecord>  {
//     let builder = &*(builder as *mut TransactionBuilder);
//     let builder = builder.get_relative_outputs();
//     // env.new_object_array()
//     builder
//
//     // Box::into_raw(Box::new(builder)) as jlong
//
// }

#[no_mangle]
/// # Safety
/// As the last operation of any transaction,
/// add a static fee to the transaction.
pub unsafe extern "system" fn Java_com_findora_JniApi_transactionBuilderAddFee(
    _env: JNIEnv,
    _: JClass,
    builder: jlong,
    inputs: jlong,
) -> jlong {
    let builder = &*(builder as *mut TransactionBuilder);
    let inputs = &*(inputs as *mut FeeInputs);
    let builder = builder.clone().add_fee(inputs.clone()).unwrap();
    Box::into_raw(Box::new(builder)) as jlong
}

#[no_mangle]
/// # Safety
/// A simple fee checker for mainnet v1.0.
///
/// SEE [check_fee](ledger::data_model::Transaction::check_fee)
pub unsafe extern "system" fn Java_com_findora_JniApi_transactionBuilderCheckFee(
    _env: JNIEnv,
    _: JClass,
    builder: jlong,
) -> jboolean {
    let builder = &*(builder as *mut TransactionBuilder);
    builder.check_fee() as jboolean
}

#[no_mangle]
/// # Safety
/// Create a new transaction builder.
/// @param {BigInt} seq_id - Unique sequence ID to prevent replay attacks.
pub unsafe extern "system" fn Java_com_findora_JniApi_transactionBuilderNew(
    _env: JNIEnv,
    _: JClass,
    seq_id: jint,
) -> jlong {
    Box::into_raw(Box::new(TransactionBuilder::new(seq_id as u64))) as jlong
}

#[no_mangle]
/// # Safety
/// Wraps around TransactionBuilder to add an asset definition operation to a transaction builder instance.
/// @example <caption> Error handling </caption>
/// try {
///     await wasm.add_operation_create_asset(wasm.new_keypair(), "test_memo", wasm.random_asset_type(), wasm.AssetRules.default());
/// } catch (err) {
///     console.log(err)
/// }
///
/// @param {XfrKeyPair} key_pair -  Issuer XfrKeyPair.
/// @param {string} memo - Text field for asset definition.
/// @param {string} token_code - Optional Base64 string representing the token code of the asset to be issued.
/// If empty, a token code will be chosen at random.
/// @param {AssetRules} asset_rules - Asset rules object specifying which simple policies apply
/// to the asset.
pub unsafe extern "system" fn Java_com_findora_JniApi_transactionBuilderAddOperationCreateAsset(
    env: JNIEnv,
    _: JClass,
    builder: jlong,
    key_pair: jlong,
    memo: JString,
    token_code: JString,
    asset_rules: jlong,
) -> jlong {
    let builder = &*(builder as *mut TransactionBuilder);
    let key_pair = &*(key_pair as *mut XfrKeyPair);
    let memo: String = env
        .get_string(memo)
        .expect("Couldn't get java string!")
        .into();
    let token_code: String = env
        .get_string(token_code)
        .expect("Couldn't get java string!")
        .into();
    let asset_rules = &*(asset_rules as *mut AssetRules);
    let builder = builder
        .clone()
        .add_operation_create_asset(key_pair, memo, token_code, asset_rules.clone())
        .unwrap();
    Box::into_raw(Box::new(builder)) as jlong
}

#[no_mangle]
/// # Safety
/// Wraps around TransactionBuilder to add an asset issuance to a transaction builder instance.
///
/// Use this function for simple one-shot issuances.
///
/// @param {XfrKeyPair} key_pair  - Issuer XfrKeyPair.
/// and types of traced assets.
/// @param {string} code - base64 string representing the token code of the asset to be issued.
/// @param {BigInt} seq_num - Issuance sequence number. Every subsequent issuance of a given asset type must have a higher sequence number than before.
/// @param {BigInt} amount - Amount to be issued.
/// @param {boolean} conf_amount - `true` means the asset amount is confidential, and `false` means it's nonconfidential.
/// @param {PublicParams} zei_params - Public parameters necessary to generate asset records.
pub unsafe extern "system" fn Java_com_findora_JniApi_transactionBuilderAddBasicIssueAsset(
    env: JNIEnv,
    _: JClass,
    builder: jlong,
    key_pair: jlong,
    code: JString,
    seq_num: jint,
    amount: jint,
    conf_amount: jboolean,
    zei_params: jlong,
) -> jlong {
    let builder = &*(builder as *mut TransactionBuilder);
    let key_pair = &*(key_pair as *mut XfrKeyPair);
    let code: String = env
        .get_string(code)
        .expect("Couldn't get java string!")
        .into();
    let zei_params = &*(zei_params as *mut PublicParams);
    let builder = builder
        .clone()
        .add_basic_issue_asset(
            key_pair,
            code,
            seq_num as u64,
            amount as u64,
            conf_amount == JNI_TRUE,
            zei_params,
        )
        .unwrap();
    Box::into_raw(Box::new(builder)) as jlong
}

#[no_mangle]
/// # Safety
/// Adds an operation to the transaction builder that adds a hash to the ledger's custom data
/// store.
/// @param {XfrKeyPair} auth_key_pair - Asset creator key pair.
/// @param {String} code - base64 string representing token code of the asset whose memo will be updated.
/// transaction validates.
/// @param {String} new_memo - The new asset memo.
/// @see {@link module:Findora-Wasm~AssetRules#set_updatable|AssetRules.set_updatable} for more information about how
/// to define an updatable asset.
pub unsafe extern "system" fn Java_com_findora_JniApi_transactionBuilderAddOperationUpdateMemo(
    env: JNIEnv,
    _: JClass,
    builder: jlong,
    auth_key_pair: jlong,
    code: JString,
    new_memo: JString,
) -> jlong {
    let builder = &*(builder as *mut TransactionBuilder);
    let auth_key_pair = &*(auth_key_pair as *mut XfrKeyPair);
    let code: String = env
        .get_string(code)
        .expect("Couldn't get java string!")
        .into();
    let new_memo: String = env
        .get_string(new_memo)
        .expect("Couldn't get java string!")
        .into();
    let builder = builder
        .clone()
        .add_operation_update_memo(auth_key_pair, code, new_memo)
        .unwrap();
    Box::into_raw(Box::new(builder)) as jlong
}

#[no_mangle]
/// # Safety
///
pub unsafe extern "system" fn Java_com_findora_JniApi_transactionBuilderAddOperationDelegate(
    env: JNIEnv,
    _: JClass,
    builder: jlong,
    keypair: jlong,
    amount: JString,
    validator: JString,
) -> jlong {
    let builder = &*(builder as *mut TransactionBuilder);
    let keypair = &*(keypair as *mut XfrKeyPair);
    let validator: String = env
        .get_string(validator)
        .expect("Couldn't get java string!")
        .into();
    let am: String = env
        .get_string(amount)
        .expect("Couldn't get java string!")
        .into();

    let builder = builder
        .clone()
        .add_operation_delegate(keypair, am.parse::<u64>().unwrap(), validator)
        .unwrap();
    Box::into_raw(Box::new(builder)) as jlong
}

#[no_mangle]
/// # Safety
///
pub unsafe extern "system" fn Java_com_findora_JniApi_transactionBuilderAddOperationUndelegate(
    _env: JNIEnv,
    _: JClass,
    builder: jlong,
    keypair: jlong,
) -> jlong {
    let builder = &*(builder as *mut TransactionBuilder);
    let keypair = &*(keypair as *mut XfrKeyPair);
    let builder = builder.clone().add_operation_undelegate(keypair).unwrap();
    Box::into_raw(Box::new(builder)) as jlong
}

#[no_mangle]
/// # Safety
///
pub unsafe extern "system" fn Java_com_findora_JniApi_transactionBuilderAddOperationUndelegatePartially(
    env: JNIEnv,
    _: JClass,
    builder: jlong,
    keypair: jlong,
    am: JString,
    validator: JString,
) -> jlong {
    let builder = &*(builder as *mut TransactionBuilder);
    let keypair = &*(keypair as *mut XfrKeyPair);
    let am: String = env
        .get_string(am)
        .expect("Couldn't get java string!")
        .into();

    let validator: String = env
        .get_string(validator)
        .expect("Couldn't get java string!")
        .into();
    let builder = builder
        .clone()
        .add_operation_undelegate_partially(
            keypair,
            am.parse::<u64>().unwrap(),
            validator,
        )
        .unwrap();
    Box::into_raw(Box::new(builder)) as jlong
}

#[no_mangle]
/// # Safety
///
pub unsafe extern "system" fn Java_com_findora_JniApi_transactionBuilderAddOperationClaim(
    _env: JNIEnv,
    _: JClass,
    builder: jlong,
    keypair: jlong,
) -> jlong {
    let builder = &*(builder as *mut TransactionBuilder);
    let keypair = &*(keypair as *mut XfrKeyPair);
    let builder = builder.clone().add_operation_claim(keypair).unwrap();
    Box::into_raw(Box::new(builder)) as jlong
}

#[no_mangle]
/// # Safety
///
pub unsafe extern "system" fn Java_com_findora_JniApi_transactionBuilderAddOperationClaimCustom(
    _env: JNIEnv,
    _: JClass,
    builder: jlong,
    keypair: jlong,
    am: jint,
) -> jlong {
    let builder = &*(builder as *mut TransactionBuilder);
    let keypair = &*(keypair as *mut XfrKeyPair);
    let builder = builder
        .clone()
        .add_operation_claim_custom(keypair, am as u64)
        .unwrap();
    Box::into_raw(Box::new(builder)) as jlong
}

#[no_mangle]
/// # Safety
///
/// Adds a serialized transfer asset operation to a transaction builder instance.
/// @param {string} op - a JSON-serialized transfer operation.
/// @see {@link module:Findora-Wasm~TransferOperationBuilder} for details on constructing a transfer operation.
/// @throws Will throw an error if `op` fails to deserialize.
pub unsafe extern "system" fn Java_com_findora_JniApi_transactionBuilderAddTransferOperation(
    env: JNIEnv,
    _: JClass,
    builder: jlong,
    op: JString,
) -> jlong {
    let builder = &*(builder as *mut TransactionBuilder);
    let op: String = env
        .get_string(op)
        .expect("Couldn't get java string!")
        .into();
    let builder = builder.clone().add_transfer_operation(op).unwrap();
    Box::into_raw(Box::new(builder)) as jlong
}

#[no_mangle]
/// # Safety
///
/// Adds a serialized transfer-account operation to transaction builder instance.
/// @param {string} amount - amount to transfer.
/// @param {XfrKeyPair} keypair - FRA account key pair.
/// @param {String} address - FRA account key pair.
/// @throws Will throw an error if `address` is invalid.
pub unsafe extern "system" fn Java_com_findora_JniApi_transactionBuilderAddTransferToAccount(
    env: JNIEnv,
    _: JClass,
    builder: jlong,
    amount: jint,
    keypair: jlong,
    address: JString,
) -> jlong {
    let builder = &*(builder as *mut TransactionBuilder);
    let addr: String = env
        .get_string(address)
        .expect("Couldn't get java string!")
        .into();

    let fra_kp = &*(keypair as *mut XfrKeyPair);

    builder
        .clone()
        .add_transfer_to_account_operation(amount as u64, Some(addr), fra_kp)
        .unwrap();
    Box::into_raw(Box::new(builder)) as jlong
}

#[no_mangle]
/// # Safety
///
pub unsafe extern "system" fn Java_com_findora_JniApi_transactionBuilderSign(
    _env: JNIEnv,
    _: JClass,
    builder: jlong,
    kp: jlong,
) -> jlong {
    let builder = &*(builder as *mut TransactionBuilder);
    let kp = &*(kp as *mut XfrKeyPair);
    let builder = builder.clone().sign(kp).unwrap();
    Box::into_raw(Box::new(builder)) as jlong
}

#[no_mangle]
/// # Safety
///
/// Extracts the serialized form of a transaction.
pub unsafe extern "system" fn Java_com_findora_JniApi_transactionBuilderTransaction(
    env: JNIEnv,
    _: JClass,
    builder: jlong,
) -> jstring {
    let builder = &*(builder as *mut TransactionBuilder);
    let output = env
        .new_string(builder.transaction())
        .expect("Couldn't create java string!");
    output.into_inner()
}

#[no_mangle]
/// # Safety
///
/// Calculates transaction handle.
pub unsafe extern "system" fn Java_com_findora_JniApi_transactionBuilderTransactionHandle(
    env: JNIEnv,
    _: JClass,
    builder: jlong,
) -> jstring {
    let builder = &*(builder as *mut TransactionBuilder);
    let output = env
        .new_string(builder.transaction_handle())
        .expect("Couldn't create java string!");
    output.into_inner()
}

#[no_mangle]
/// # Safety
///
/// Fetches a client record from a transaction.
/// @param {number} idx - Record to fetch. Records are added to the transaction builder sequentially.
pub unsafe extern "system" fn Java_com_findora_JniApi_transactionBuilderGetOwnerRecord(
    _env: JNIEnv,
    _: JClass,
    builder: jlong,
    idx: jint,
) -> jlong {
    let builder = &*(builder as *mut TransactionBuilder);
    Box::into_raw(Box::new(builder.get_owner_record(idx as usize))) as jlong
}

#[no_mangle]
/// # Safety
///
/// Fetches an owner memo from a transaction
/// @param {number} idx - Owner memo to fetch. Owner memos are added to the transaction builder sequentially.
pub unsafe extern "system" fn Java_com_findora_JniApi_transactionBuilderGetOwnerMemo(
    _env: JNIEnv,
    _: JClass,
    builder: jlong,
    idx: jint,
) -> jlong {
    let builder = &*(builder as *mut TransactionBuilder);
    Box::into_raw(Box::new(builder.get_owner_memo(idx as usize))) as jlong
}
