use crate::rust::*;
use jni::objects::{JClass, JString};
use jni::sys::{jboolean, jint, jlong, jstring, jvalue, JNI_TRUE};
use jni::JNIEnv;
use ledger::data_model::AssetType as PlatformAssetType;
use zei::noah_api::keys::{KeyPair, PublicKey};
use zei::noah_api::xfr::structs::OwnerMemo as NoahOwnerMemo;
use zei::{XfrKeyPair, XfrPublicKey};

use super::{jStringToString, parseU64};

#[no_mangle]
/// # Safety
/// Builds an asset type from a JSON-encoded JavaScript value.
/// @param {JsValue} val - JSON-encoded asset type fetched from ledger server with the `asset_token/{code}` route.
/// Note: The first field of an asset type is `properties`. See the example below.
///
/// @example
/// "properties":{
///   "code":{
///     "val":[151,8,106,38,126,101,250,236,134,77,83,180,43,152,47,57,83,30,60,8,132,218,48,52,167,167,190,244,34,45,78,80]
///   },
///   "issuer":{"key":“iFW4jY_DQVSGED05kTseBBn0BllPB9Q9escOJUpf4DY=”},
///   "memo":“test memo”,
///   "asset_rules":{
///     "transferable":true,
///     "updatable":false,
///     "transfer_multisig_rules":null,
///     "max_units":5000
///   }
/// }
///
/// @see {@link module:Findora-Network~Network#getAssetProperties|Network.getAsset} for information about how to
/// fetch an asset type from the ledger server.
pub unsafe extern "system" fn Java_com_findora_JniApi_assetTypeFromJson(
    env: JNIEnv,
    _: JClass,
    asset_type_json: JString,
) -> jlong {
    let asset_type_json: String = env
        .get_string(asset_type_json)
        .expect("Couldn't get java string!")
        .into();

    let asset_type: PlatformAssetType =
        serde_json::from_str(asset_type_json.as_str()).unwrap();
    Box::into_raw(Box::new(AssetType::from_json(asset_type).unwrap())) as jlong
}

#[no_mangle]
/// # Safety
/// Fetch the tracing policies associated with this asset type.
/// @returns {TracingPolicies}
pub unsafe extern "system" fn Java_com_findora_JniApi_assetTypeGetTracingPolicies(
    _env: JNIEnv,
    _: JClass,
    asset_type: jlong,
) -> jlong {
    let asset_type = &*(asset_type as *mut AssetType);
    let policy = asset_type.get_tracing_policies();
    Box::into_raw(Box::new(policy)) as jlong
}

#[no_mangle]
/// # Safety
/// Converts a base64 encoded public key string to a public key.
/// @param {string} pk
/// @returns {XfrPublicKey}
pub unsafe extern "system" fn Java_com_findora_JniApi_publicKeyFromBase64(
    env: JNIEnv,
    _: JClass,
    pk: JString,
) -> jlong {
    let pk: String = env
        .get_string(pk)
        .expect("Couldn't get java string!")
        .into();

    let key = rs_public_key_from_base64(pk.as_str()).unwrap();
    Box::into_raw(Box::new(key)) as jlong
}

#[no_mangle]
/// # Safety
/// Creates a relative txo reference as a JSON string. Relative txo references are offset
/// backwards from the operation they appear in -- 0 is the most recent, (n-1) is the first output
/// of the transaction.
///
/// Use relative txo indexing when referring to outputs of intermediate operations (e.g. a
/// transaction containing both an issuance and a transfer).
///
/// # Arguments
/// @param {BigInt} idx -  Relative TXO (transaction output) SID.
/// @returns {TxoRef}
pub unsafe extern "system" fn Java_com_findora_JniApi_txoRefRelative(
    _env: JNIEnv,
    _: JClass,
    idx: jint,
) -> jlong {
    Box::into_raw(Box::new(TxoRef::relative(idx as u64))) as jlong
}

#[no_mangle]
/// # Safety
/// Creates an absolute transaction reference as a JSON string.
///
/// Use absolute txo indexing when referring to an output that has been assigned a utxo index (i.e.
/// when the utxo has been committed to the ledger in an earlier transaction).
///
/// # Arguments
/// @param {BigInt} idx -  Txo (transaction output) SID.
/// @returns {TxoRef}
pub unsafe extern "system" fn Java_com_findora_JniApi_txoRefAbsolute(
    _env: JNIEnv,
    _: JClass,
    idx: jint,
) -> jlong {
    Box::into_raw(Box::new(TxoRef::absolute(idx as u64))) as jlong
}

#[no_mangle]
/// # Safety
/// Builds a client record from a JSON-encoded JavaScript value.
///
/// @param {JsValue} val - JSON-encoded autehtnicated asset record fetched from ledger server with the `utxo_sid/{sid}` route,
/// where `sid` can be fetched from the query server with the `get_owned_utxos/{address}` route.
/// Note: The first field of an asset record is `utxo`. See the example below.
///
/// @example
/// "utxo":{
///   "amount":{
///     "NonConfidential":5
///   },
///  "asset_type":{
///     "NonConfidential":[113,168,158,149,55,64,18,189,88,156,133,204,156,46,106,46,232,62,69,233,157,112,240,132,164,120,4,110,14,247,109,127]
///   },
///   "public_key":"Glf8dKF6jAPYHzR_PYYYfzaWqpYcMvnrIcazxsilmlA="
/// }
///
/// @see {@link module:Findora-Network~Network#getUtxo|Network.getUtxo} for information about how to
/// fetch an asset record from the ledger server.
pub unsafe extern "system" fn Java_com_findora_JniApi_clientAssetRecordFromJson(
    env: JNIEnv,
    _: JClass,
    val: JString,
) -> jlong {
    let val: String = env
        .get_string(val)
        .expect("Couldn't get java string!")
        .into();

    Box::into_raw(Box::new(
        ClientAssetRecord::from_json(val.as_str()).unwrap(),
    )) as jlong
}

#[no_mangle]
/// # Safety
/// ClientAssetRecord ==> JsValue
pub unsafe extern "system" fn Java_com_findora_JniApi_clientAssetRecordToJson(
    env: JNIEnv,
    _: JClass,
    record: jlong,
) -> jstring {
    let record = &*(record as *mut ClientAssetRecord);
    let output = env
        .new_string(record.to_json().unwrap())
        .expect("Couldn't create java string!");
    **output
}

#[no_mangle]
/// # Safety
/// Builds an owner memo from a JSON-serialized JavaScript value.
/// @param {JsValue} val - JSON owner memo fetched from query server with the `get_owner_memo/{sid}` route,
/// where `sid` can be fetched from the query server with the `get_owned_utxos/{address}` route. See the example below.
///
/// @example
/// {
///   "blind_share":[91,251,44,28,7,221,67,155,175,213,25,183,70,90,119,232,212,238,226,142,159,200,54,19,60,115,38,221,248,202,74,248],
///   "lock":{"ciphertext":[119,54,117,136,125,133,112,193],"encoded_rand":"8KDql2JphPB5WLd7-aYE1bxTQAcweFSmrqymLvPDntM="}
/// }
pub unsafe extern "system" fn Java_com_findora_JniApi_ownerMemoFromJson(
    env: JNIEnv,
    _: JClass,
    val: JString,
) -> jlong {
    let val: String = env
        .get_string(val)
        .expect("Couldn't get java string!")
        .into();

    let noah_owner_memo: NoahOwnerMemo = serde_json::from_str(val.as_str()).unwrap();
    Box::into_raw(Box::new(OwnerMemo::from_json(noah_owner_memo).unwrap())) as jlong
}

#[no_mangle]
/// # Safety
///
pub unsafe extern "system" fn Java_com_findora_JniApi_assetTracerKeyPairNew(
    _env: JNIEnv,
    _: JClass,
) -> jlong {
    Box::into_raw(Box::new(AssetTracerKeyPair::new())) as jlong
}

#[no_mangle]
/// # Safety
/// Create a new transfer operation builder.
/// @returns {TransferOperationBuilder}
pub extern "system" fn Java_com_findora_JniApi_transferOperationBuilderNew(
    _env: JNIEnv,
    _: JClass,
) -> jlong {
    Box::into_raw(Box::new(TransferOperationBuilder::new())) as jlong
}

#[no_mangle]
/// # Safety
/// Debug function that does not need to go into the docs.
pub unsafe extern "system" fn Java_com_findora_JniApi_transferOperationBuilderDebug(
    env: JNIEnv,
    _: JClass,
    builder: jlong,
) -> jstring {
    let builder = &mut *(builder as *mut TransferOperationBuilder);
    let output = env
        .new_string(builder.debug())
        .expect("Couldn't create java string!");
    **output
}

#[no_mangle]
/// # Safety
/// Wraps around TransferOperationBuilder to add an input to a transfer operation builder.
//  @param {TxoRef} txo_ref - Absolute or relative utxo reference
//  @param {string} asset_record - Serialized client asset record to serve as transfer input. This record must exist on the
//  ledger for the transfer to be valid.
//  @param {OwnerMemo} owner_memo - Opening parameters.
//  @param tracing_key {AssetTracerKeyPair} - Tracing key, must be added to traceable
//  assets.
//  @param {XfrKeyPair} key - Key pair associated with the input.
//  @param {BigInt} amount - Amount of input record to transfer.
//  @see {@link module:Findora-Wasm~TxoRef#create_absolute_txo_ref|TxoRef.create_absolute_txo_ref}
//  or {@link module:Findora-Wasm~TxoRef#create_relative_txo_ref|TxoRef.create_relative_txo_ref} for details on txo
//  references.
//  @see {@link module:Findora-Network~Network#getUtxo|Network.getUtxo} for details on fetching blind asset records.
//  @throws Will throw an error if `oar` or `txo_ref` fail to deserialize.
//  @param {TxoRef} txo_ref
//  @param {ClientAssetRecord} asset_record
//  @param {OwnerMemo | undefined} owner_memo
//  @param {TracingPolicies} tracing_policies
//  @param {XfrKeyPair} key
//  @param {BigInt} amount
//  @returns {TransferOperationBuilder}
pub unsafe extern "system" fn Java_com_findora_JniApi_transferOperationBuilderAddInputWithTracing(
    env: JNIEnv,
    _: JClass,
    builder: jlong,
    txo_ref_ptr: jlong,
    asset_record_ptr: jlong,
    owner_memo_ptr: jlong,
    tracing_policies_ptr: jlong,
    key_ptr: jlong,
    amount: JString,
) -> jlong {
    let builder = &*(builder as *mut TransferOperationBuilder);
    let txo_ref = *(txo_ref_ptr as *mut TxoRef);
    let asset_record = &*(asset_record_ptr as *mut ClientAssetRecord);
    let owner_memo = if 0 == owner_memo_ptr {
        None
    } else {
        let memo = &*(owner_memo_ptr as *mut OwnerMemo);
        Some(memo.clone())
    };
    let tracing_policies = &*(tracing_policies_ptr as *mut TracingPolicies);
    let key = &*(key_ptr as *mut KeyPair);
    let amount = parseU64(env, amount);

    let builder = builder
        .clone()
        .add_input_with_tracing(
            txo_ref,
            asset_record.clone(),
            owner_memo,
            tracing_policies,
            &XfrKeyPair::from_noah(key).unwrap(),
            amount,
        )
        .unwrap();
    Box::into_raw(Box::new(builder)) as jlong
}

#[no_mangle]
/// # Safety
/// Wraps around TransferOperationBuilder to add an input to a transfer operation builder.
// * @param {TxoRef} txo_ref - Absolute or relative utxo reference
// * @param {string} asset_record - Serialized client asset record to serve as transfer input. This record must exist on the
// * ledger for the transfer to be valid
// * @param {OwnerMemo} owner_memo - Opening parameters.
// * @param {XfrKeyPair} key - Key pair associated with the input.
// * @param {BigInt} amount - Amount of input record to transfer
// * or {@link module:Findora-Wasm~TxoRef#create_relative_txo_ref|TxoRef.create_relative_txo_ref} for details on txo
// * references.
// * @see {@link module:Findora-Network~Network#getUtxo|Network.getUtxo} for details on fetching blind asset records.
// * @throws Will throw an error if `oar` or `txo_ref` fail to deserialize.
// * @param {TxoRef} txo_ref
// * @param {ClientAssetRecord} asset_record
// * @param {OwnerMemo | undefined} owner_memo
// * @param {XfrKeyPair} key
// * @param {BigInt} amount
// * @returns {TransferOperationBuilder}
pub unsafe extern "system" fn Java_com_findora_JniApi_transferOperationBuilderAddInputNoTracing(
    env: JNIEnv,
    _: JClass,
    builder: jlong,
    txo_ref_ptr: jlong,
    asset_record_ptr: jlong,
    owner_memo_ptr: jlong,
    key_ptr: jlong,
    amount: JString,
) -> jlong {
    let builder = &*(builder as *mut TransferOperationBuilder);
    let txo_ref = *(txo_ref_ptr as *mut TxoRef);
    let asset_record = &*(asset_record_ptr as *mut ClientAssetRecord);
    let owner_memo = if 0 == owner_memo_ptr {
        None
    } else {
        let memo = &*(owner_memo_ptr as *mut OwnerMemo);
        Some(memo.clone())
    };
    let key = &*(key_ptr as *mut KeyPair);
    let amount = parseU64(env, amount);

    let builder = builder
        .clone()
        .add_input_no_tracing(
            txo_ref,
            asset_record,
            owner_memo,
            &XfrKeyPair::from_noah(key).unwrap(),
            amount,
        )
        .unwrap();
    Box::into_raw(Box::new(builder)) as jlong
}

#[no_mangle]
/// # Safety
/// Wraps around TransferOperationBuilder to add an output to a transfer operation builder.
// * @param {BigInt} amount - amount to transfer to the recipient.
// * @param {XfrPublicKey} recipient - public key of the recipient.
// * @param tracing_key {AssetTracerKeyPair} - Optional tracing key, must be added to traced
// * assets.
// * @param code {string} - String representation of the asset token code.
// * @param conf_amount {boolean} - `true` means the output's asset amount is confidential, and `false` means it's nonconfidential.
// * @param conf_type {boolean} - `true` means the output's asset type is confidential, and `false` means it's nonconfidential.
// * @throws Will throw an error if `code` fails to deserialize.
// * @param {BigInt} amount
// * @param {XfrPublicKey} recipient
// * @param {TracingPolicies} tracing_policies
// * @param {string} code
// * @param {boolean} conf_amount
// * @param {boolean} conf_type
// * @returns {TransferOperationBuilder}
pub unsafe extern "system" fn Java_com_findora_JniApi_transferOperationBuilderAddOutputWithTracing(
    env: JNIEnv,
    _: JClass,
    builder: jlong,
    amount: JString,
    recipient: jlong,
    tracing_policies_ptr: jlong,
    code: JString,
    conf_amount: jboolean,
    conf_type: jboolean,
) -> jlong {
    let builder = &*(builder as *mut TransferOperationBuilder);
    let tracing_policies = &*(tracing_policies_ptr as *mut TracingPolicies);
    let recipient = &*(recipient as *mut PublicKey);
    let amount = parseU64(env, amount);
    let code = jStringToString(env, code);

    let builder = builder
        .clone()
        .add_output_with_tracing(
            amount,
            &XfrPublicKey::from_noah(recipient).unwrap(),
            tracing_policies,
            code,
            conf_amount == JNI_TRUE,
            conf_type == JNI_TRUE,
        )
        .unwrap();
    Box::into_raw(Box::new(builder)) as jlong
}

#[no_mangle]
/// # Safety
/// Wraps around TransferOperationBuilder to add an output to a transfer operation builder.
// * @param {BigInt} amount - amount to transfer to the recipient
// * @param {XfrPublicKey} recipient - public key of the recipient
// * @param code {string} - String representaiton of the asset token code
// * @param conf_amount {boolean} - `true` means the output's asset amount is confidential, and `false` means it's nonconfidential.
// * @param conf_type {boolean} - `true` means the output's asset type is confidential, and `false` means it's nonconfidential.
// * @throws Will throw an error if `code` fails to deserialize.
// * @param {BigInt} amount
// * @param {XfrPublicKey} recipient
// * @param {string} code
// * @param {boolean} conf_amount
// * @param {boolean} conf_type
// * @returns {TransferOperationBuilder}
pub unsafe extern "system" fn Java_com_findora_JniApi_transferOperationBuilderAddOutputNoTracing(
    env: JNIEnv,
    _: JClass,
    builder: jlong,
    amount: JString,
    recipient: jlong,
    code: JString,
    conf_amount: jboolean,
    conf_type: jboolean,
) -> jlong {
    let builder = &*(builder as *mut TransferOperationBuilder);
    let recipient = &*(recipient as *mut PublicKey);
    let amount = parseU64(env, amount);
    let code = jStringToString(env, code);

    let builder = builder
        .clone()
        .add_output_no_tracing(
            amount,
            &XfrPublicKey::from_noah(recipient).unwrap(),
            code,
            conf_amount == JNI_TRUE,
            conf_type == JNI_TRUE,
        )
        .unwrap();
    Box::into_raw(Box::new(builder)) as jlong
}

#[no_mangle]
/// # Safety
///
pub unsafe extern "system" fn Java_com_findora_JniApi_transferOperationBuilderAddInput(
    env: JNIEnv,
    _: JClass,
    builder: jlong,
    txo_ref_ptr: jlong,
    asset_record_ptr: jlong,
    owner_memo_ptr: jlong,
    tracing_policies_ptr: jlong,
    key_ptr: jlong,
    amount: JString,
) -> jlong {
    let builder = &*(builder as *mut TransferOperationBuilder);
    let txo_ref = *(txo_ref_ptr as *mut TxoRef);
    let asset_record = &*(asset_record_ptr as *mut ClientAssetRecord);
    let owner_memo = if 0 == owner_memo_ptr {
        None
    } else {
        let memo = &*(owner_memo_ptr as *mut OwnerMemo);
        Some(memo.clone())
    };
    let tracing_policies = if 0 == tracing_policies_ptr {
        None
    } else {
        let policies = &*(tracing_policies_ptr as *mut TracingPolicies);
        Some(policies)
    };
    let key = &*(key_ptr as *mut KeyPair);

    let builder = builder
        .clone()
        .add_input(
            txo_ref,
            asset_record,
            owner_memo,
            tracing_policies,
            &XfrKeyPair::from_noah(key).unwrap(),
            parseU64(env, amount),
        )
        .unwrap();
    Box::into_raw(Box::new(builder)) as jlong
}

#[no_mangle]
/// # Safety
///
pub unsafe extern "system" fn Java_com_findora_JniApi_transferOperationBuilderAddOutput(
    env: JNIEnv,
    _: JClass,
    builder: jlong,
    amount: JString,
    recipient: jlong,
    tracing_policies_ptr: jlong,
    code: JString,
    conf_amount: jboolean,
    conf_type: jboolean,
) -> jlong {
    let builder = &*(builder as *mut TransferOperationBuilder);
    let tracing_policies = if 0 == tracing_policies_ptr {
        None
    } else {
        let policies = &*(tracing_policies_ptr as *mut TracingPolicies);
        Some(policies)
    };
    let recipient = &*(recipient as *mut PublicKey);
    let code: String = env
        .get_string(code)
        .expect("Couldn't get java string!")
        .into();

    let builder = builder
        .clone()
        .add_output(
            parseU64(env, amount),
            &XfrPublicKey::from_noah(&recipient).unwrap(),
            tracing_policies,
            code,
            conf_amount == JNI_TRUE,
            conf_type == JNI_TRUE,
        )
        .unwrap();
    Box::into_raw(Box::new(builder)) as jlong
}

#[no_mangle]
/// # Safety
/// Wraps around TransferOperationBuilder to ensure the transfer inputs and outputs are balanced.
/// This function will add change outputs for all unspent portions of input records.
/// @throws Will throw an error if the transaction cannot be balanced.
/// @returns {TransferOperationBuilder}
pub unsafe extern "system" fn Java_com_findora_JniApi_transferOperationBuilderBalance(
    _env: JNIEnv,
    _: JClass,
    builder: jlong,
) -> jlong {
    let builder = &*(builder as *mut TransferOperationBuilder);
    Box::into_raw(Box::new(builder.clone().balance(None).unwrap())) as jlong
}

#[no_mangle]
/// # Safety
/// Wraps around TransferOperationBuilder to finalize the transaction.
/// @throws Will throw an error if input and output amounts do not add up.
/// @throws Will throw an error if not all record owners have signed the transaction.
/// @returns {TransferOperationBuilder}
pub unsafe extern "system" fn Java_com_findora_JniApi_transferOperationBuilderCreate(
    _env: JNIEnv,
    _: JClass,
    builder: jlong,
) -> jlong {
    let builder = &*(builder as *mut TransferOperationBuilder);
    Box::into_raw(Box::new(builder.clone().create().unwrap())) as jlong
}

#[no_mangle]
/// # Safety
/// Wraps around TransferOperationBuilder to add a signature to the operation.
///
/// All input owners must sign.
///
/// @param {XfrKeyPair} kp - key pair of one of the input owners.
/// @param {XfrKeyPair} kp
/// @returns {TransferOperationBuilder}
pub unsafe extern "system" fn Java_com_findora_JniApi_transferOperationBuilderSign(
    _env: JNIEnv,
    _: JClass,
    builder: jlong,
    key_ptr: jlong,
) -> jlong {
    let builder = &*(builder as *mut TransferOperationBuilder);
    let key = &*(key_ptr as *mut KeyPair);

    Box::into_raw(Box::new(
        builder
            .clone()
            .sign(&XfrKeyPair::from_noah(&key).unwrap())
            .unwrap(),
    )) as jlong
}

#[no_mangle]
/// # Safety
///
pub unsafe extern "system" fn Java_com_findora_JniApi_transferOperationBuilderBuilder(
    env: JNIEnv,
    _: JClass,
    builder: jlong,
) -> jstring {
    let builder = &*(builder as *mut TransferOperationBuilder);
    let output = env
        .new_string(builder.builder())
        .expect("Couldn't create java string!");
    **output
}

#[no_mangle]
/// # Safety
/// Wraps around TransferOperationBuilder to extract an operation expression as JSON.
pub unsafe extern "system" fn Java_com_findora_JniApi_transferOperationBuilderTransaction(
    env: JNIEnv,
    _: JClass,
    builder: jlong,
) -> jstring {
    let builder = &*(builder as *mut TransferOperationBuilder);
    let output = env
        .new_string(builder.transaction().unwrap())
        .expect("Couldn't create java string!");
    **output
}

#[no_mangle]
/// # Safety
/// Fee smaller than this value will be denied.
/// @returns {BigInt}
pub unsafe extern "system" fn Java_com_findora_JniApi_fraGetMinimalFee(
    _env: JNIEnv,
    _: JClass,
) -> jvalue {
    jvalue {
        j: fra_get_minimal_fee() as jlong,
    }
}

#[no_mangle]
/// # Safety
/// The destination for fee to be transfered to.
/// @returns {XfrPublicKey}
pub unsafe extern "system" fn Java_com_findora_JniApi_fraGetDestPubkey(
    _env: JNIEnv,
    _: JClass,
) -> jlong {
    Box::into_raw(Box::new(fra_get_dest_pubkey())) as jlong
}

#[no_mangle]
/// # Safety
/// The system address used to reveive delegation principals.
pub unsafe extern "system" fn Java_com_findora_JniApi_getDelegationTargetAddress(
    env: JNIEnv,
    _: JClass,
) -> jstring {
    let output = env
        .new_string(get_delegation_target_address())
        .expect("Couldn't create java string!");
    **output
}

#[no_mangle]
/// # Safety
///
pub unsafe extern "system" fn Java_com_findora_JniApi_getCoinbaseAddress(
    env: JNIEnv,
    _: JClass,
) -> jstring {
    let output = env
        .new_string(get_coinbase_address())
        .expect("Couldn't create java string!");
    **output
}

#[no_mangle]
/// # Safety
///
pub unsafe extern "system" fn Java_com_findora_JniApi_getCoinbasePrincipalAddress(
    env: JNIEnv,
    _: JClass,
) -> jstring {
    let output = env
        .new_string(get_coinbase_principal_address())
        .expect("Couldn't create java string!");
    **output
}

#[no_mangle]
/// # Safety
///
pub unsafe extern "system" fn Java_com_findora_JniApi_getDelegationMinAmount(
    _env: JNIEnv,
    _: JClass,
) -> jvalue {
    jvalue {
        j: get_delegation_min_amount() as jlong,
    }
}

#[no_mangle]
/// # Safety
///
pub unsafe extern "system" fn Java_com_findora_JniApi_getDelegationMaxAmount(
    _env: JNIEnv,
    _: JClass,
) -> jvalue {
    jvalue {
        j: get_delegation_max_amount() as jlong,
    }
}
