#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

use super::data_model::*;
use aes_gcm::aead::{generic_array::GenericArray, Aead, KeyInit};
use aes_gcm::Aes256Gcm;
use credentials::{
    credential_commit, credential_issuer_key_gen, credential_open_commitment,
    credential_reveal, credential_sign, credential_user_key_gen, credential_verify,
    credential_verify_commitment, CredIssuerPublicKey, CredIssuerSecretKey,
    CredUserPublicKey, CredUserSecretKey, Credential as PlatformCredential,
};
use cryptohash::sha256;
use getrandom::getrandom;
use globutils::wallet;
use ledger::{
    data_model::{
        AssetTypeCode, ASSET_TYPE_FRA, BLACK_HOLE_PUBKEY, BLACK_HOLE_PUBKEY_STAKING,
        TX_FEE_MIN,
    },
    staking::{MAX_DELEGATION_AMOUNT, MIN_DELEGATION_AMOUNT},
};
use noah::xfr::asset_record::open_blind_asset_record as open_bar;
use noah::xfr::sig::{XfrKeyPair, XfrPublicKey, XfrSecretKey};
use noah::xfr::structs::{
    AssetType as NoahAssetType, OpenAssetRecord, XfrBody, ASSET_TYPE_LENGTH,
};
use noah::xfr::trace_assets as noah_trace_assets;
use noah_algebra::serialization::NoahFromToBytes;
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use ring::pbkdf2;
use ruc::Result;
use std::num::NonZeroU32;
use std::str;

#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
/// Generates random Base64 encoded asset type as a Base64 string. Used in asset definitions.
/// @see {@link
/// module:Findora-Wasm~TransactionBuilder#add_operation_create_asset|add_operation_create_asset}
/// for instructions on how to define an asset with a new
/// asset type
pub fn random_asset_type() -> String {
    AssetTypeCode::gen_random().to_base64()
}

/// Generates asset type as a Base64 string from given code.
pub fn rs_asset_type_from_value(code: [u8; ASSET_TYPE_LENGTH]) -> String {
    AssetTypeCode {
        val: NoahAssetType(code),
    }
    .to_base64()
}

/// Returns information about traceable assets for a given transfer.
pub fn rs_trace_assets(
    xfr_body: XfrBody,
    tracer_keypair: &AssetTracerKeyPair,
) -> Result<Vec<(u64, String)>> {
    Ok(noah_trace_assets(&xfr_body, tracer_keypair.get_keys())?
        .iter()
        .map(|(amt, asset_type, _, _)| {
            let asset_type_code = AssetTypeCode { val: *asset_type };
            (*amt, asset_type_code.to_base64())
        })
        .collect())
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
// Testnet will not support direct API access to hardcoded debt policy.
/// Returns an address to use for cancelling debt tokens in a debt swap.
/// @ignore
pub fn get_null_pk() -> XfrPublicKey {
    XfrPublicKey::noah_from_bytes(&[0; 32]).unwrap()
}

/// Returns a JavaScript object containing decrypted owner record information,
/// where `amount` is the decrypted asset amount, and `asset_type` is the decrypted asset type code.
pub fn rs_open_client_asset_record(
    record: &ClientAssetRecord,
    owner_memo: Option<OwnerMemo>,
    keypair: &XfrKeyPair,
) -> Result<OpenAssetRecord> {
    open_bar(
        record.get_bar_ref(),
        &owner_memo.map(|memo| memo.get_memo_ref().clone()),
        keypair,
    )
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
/// Extracts the public key as a string from a transfer key pair.
pub fn get_pub_key_str(key_pair: &XfrKeyPair) -> String {
    serde_json::to_string(key_pair.get_pk_ref()).unwrap()
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
/// Extracts the private key as a string from a transfer key pair.
pub fn get_priv_key_str(key_pair: &XfrKeyPair) -> String {
    serde_json::to_string(key_pair.get_sk_ref()).unwrap()
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
/// Creates a new transfer key pair.
pub fn new_keypair() -> XfrKeyPair {
    let mut prng = ChaChaRng::from_entropy();
    XfrKeyPair::generate(&mut prng)
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
/// Generates a new keypair deterministically from a seed string and an optional name.
pub fn new_keypair_from_seed(seed_str: String, name: Option<String>) -> XfrKeyPair {
    let seed_str = seed_str + &name.unwrap_or_default();
    let hash = sha256::hash(seed_str.as_bytes());
    let mut prng = ChaChaRng::from_seed(hash.0);
    XfrKeyPair::generate(&mut prng)
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
/// Returns base64 encoded representation of an XfrPublicKey.
pub fn public_key_to_base64(key: &XfrPublicKey) -> String {
    wallet::public_key_to_base64(key)
}

/// Converts a base64 encoded public key string to a public key.
pub fn rs_public_key_from_base64(pk: &str) -> Result<XfrPublicKey> {
    wallet::public_key_from_base64(pk)
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
/// Expresses a transfer key pair as a hex-encoded string.
/// To decode the string, use `keypair_from_str` function.
pub fn keypair_to_str(key_pair: &XfrKeyPair) -> String {
    hex::encode(key_pair.noah_to_bytes())
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
/// Constructs a transfer key pair from a hex-encoded string.
/// The encode a key pair, use `keypair_to_str` function.
pub fn keypair_from_str(str: String) -> XfrKeyPair {
    XfrKeyPair::noah_from_bytes(&hex::decode(str).unwrap()).unwrap()
}

/// Generates a new credential issuer key.
pub fn rs_wasm_credential_issuer_key_gen(
    mut attributes: Vec<AttributeDefinition>,
) -> CredentialIssuerKeyPair {
    let mut prng = ChaChaRng::from_entropy();
    let attributes: Vec<(String, usize)> = attributes
        .drain(..)
        .map(|attr| (attr.name, attr.size))
        .collect();

    let (pk, sk) = credential_issuer_key_gen(&mut prng, &attributes[..]);
    CredentialIssuerKeyPair { pk, sk }
}

/// Verifies a credential commitment. Used to confirm that a credential is tied to a ledger
/// address.
pub fn rs_wasm_credential_verify_commitment(
    issuer_pub_key: &CredIssuerPublicKey,
    commitment: &CredentialCommitment,
    pok: &CredentialPoK,
    xfr_pk: &XfrPublicKey,
) -> Result<()> {
    credential_verify_commitment(
        issuer_pub_key,
        commitment.get_ref(),
        pok.get_ref(),
        &xfr_pk.to_bytes(),
    )
}

/// Generates a new reveal proof from a credential commitment key.
pub fn rs_wasm_credential_open_commitment(
    user_secret_key: &CredUserSecretKey,
    credential: &Credential,
    key: &CredentialCommitmentKey,
    reveal_fields: Vec<String>,
) -> Result<CredentialPoK> {
    let mut prng = ChaChaRng::from_entropy();
    let pok = credential_open_commitment(
        &mut prng,
        user_secret_key,
        credential.get_cred_ref(),
        key.get_ref(),
        &reveal_fields,
    )?;
    Ok(CredentialPoK { pok })
}

/// Generates a new credential user key.
/// @param {CredIssuerPublicKey} issuer_pub_key - The credential issuer that can sign off on this
/// user's attributes.
#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
pub fn wasm_credential_user_key_gen(
    issuer_pub_key: &CredIssuerPublicKey,
) -> CredentialUserKeyPair {
    let mut prng = ChaChaRng::from_entropy();
    let (pk, sk) = credential_user_key_gen(&mut prng, issuer_pub_key);
    CredentialUserKeyPair { pk, sk }
}

/// Generates a signature on user attributes that can be used to create a credential.
pub fn rs_wasm_credential_sign(
    issuer_secret_key: &CredIssuerSecretKey,
    user_public_key: &CredUserPublicKey,
    attributes: Vec<AttributeAssignment>,
) -> Result<CredentialSignature> {
    let mut prng = ChaChaRng::from_entropy();
    let attributes: Vec<(String, &[u8])> = attributes
        .iter()
        .map(|attr| (attr.name.clone(), attr.val.as_bytes()))
        .collect();
    let sig =
        credential_sign(&mut prng, issuer_secret_key, user_public_key, &attributes)?;
    Ok(CredentialSignature { sig })
}

/// Generates a signature on user attributes that can be used to create a credential.
pub fn rs_create_credential(
    issuer_public_key: &CredIssuerPublicKey,
    signature: &CredentialSignature,
    attributes: Vec<AttributeAssignment>,
) -> Credential {
    let attributes: Vec<(String, Vec<u8>)> = attributes
        .iter()
        .map(|attr| (attr.name.clone(), attr.val.as_bytes().to_vec()))
        .collect();
    Credential {
        credential: PlatformCredential {
            attributes,
            issuer_pub_key: issuer_public_key.clone(),
            signature: signature.get_sig_ref().clone(),
        },
    }
}

/// Generates a credential commitment. A credential commitment can be used to selectively reveal
/// attribute assignments.
pub fn rs_wasm_credential_commit(
    user_secret_key: &CredUserSecretKey,
    user_public_key: &XfrPublicKey,
    credential: &Credential,
) -> Result<CredentialCommitmentData> {
    let mut prng = ChaChaRng::from_entropy();
    let (commitment, pok, key) = credential_commit(
        &mut prng,
        user_secret_key,
        credential.get_cred_ref(),
        &user_public_key.to_bytes(),
    )?;
    Ok(CredentialCommitmentData {
        commitment: CredentialCommitment { commitment },
        pok: CredentialPoK { pok },
        commitment_key: CredentialCommitmentKey { key },
    })
}

/// Selectively reveals attributes committed to in a credential commitment
pub fn rs_wasm_credential_reveal(
    user_sk: &CredUserSecretKey,
    credential: &Credential,
    reveal_fields: Vec<String>,
) -> Result<CredentialRevealSig> {
    let mut prng = ChaChaRng::from_entropy();
    Ok(CredentialRevealSig {
        sig: credential_reveal(
            &mut prng,
            user_sk,
            credential.get_cred_ref(),
            &reveal_fields[..],
        )?,
    })
}

/// Verifies revealed attributes from a commitment.
pub fn rs_wasm_credential_verify(
    issuer_pub_key: &CredIssuerPublicKey,
    attributes: Vec<AttributeAssignment>,
    commitment: &CredentialCommitment,
    pok: &CredentialPoK,
) -> Result<()> {
    let attributes: Vec<(String, &[u8])> = attributes
        .iter()
        .map(|attr| (attr.name.clone(), attr.val.as_bytes()))
        .collect();
    credential_verify(
        issuer_pub_key,
        &attributes,
        commitment.get_ref(),
        pok.get_ref(),
    )
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
/// Returns bech32 encoded representation of an XfrPublicKey.
pub fn public_key_to_bech32(key: &XfrPublicKey) -> String {
    wallet::public_key_to_bech32(key)
}

/// Converts a bech32 encoded public key string to a public key.
pub fn rs_public_key_from_bech32(addr: &str) -> Result<XfrPublicKey> {
    wallet::public_key_from_bech32(addr)
}

pub fn rs_bech32_to_base64(pk: &str) -> Result<String> {
    let pub_key = rs_public_key_from_bech32(pk)?;
    Ok(public_key_to_base64(&pub_key))
}

pub fn rs_base64_to_bech32(pk: &str) -> Result<String> {
    let pub_key = rs_public_key_from_base64(pk)?;
    Ok(public_key_to_bech32(&pub_key))
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
pub fn encryption_pbkdf2_aes256gcm(key_pair: String, password: String) -> Vec<u8> {
    const CREDENTIAL_LEN: usize = 32;
    const IV_LEN: usize = 12;
    let n_iter = NonZeroU32::new(32).unwrap();

    let mut salt = [0u8; CREDENTIAL_LEN];
    getrandom(&mut salt).unwrap();
    let mut derived_key = [0u8; CREDENTIAL_LEN];
    pbkdf2::derive(
        pbkdf2::PBKDF2_HMAC_SHA512,
        n_iter,
        &salt,
        password.as_bytes(),
        &mut derived_key,
    );

    let mut iv = [0u8; IV_LEN];
    getrandom(&mut iv).unwrap();

    let cipher = Aes256Gcm::new(GenericArray::from_slice(&derived_key));
    let ciphertext = cipher
        .encrypt(GenericArray::from_slice(&iv), key_pair.as_ref())
        .unwrap_or_default();

    // this is a hack, wasm-bindgen not support tuple of vectors
    let mut res: Vec<u8> = Vec::new();
    res.append(&mut salt.to_vec());
    res.append(&mut iv.to_vec());
    res.append(&mut ciphertext.to_vec());
    res
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
pub fn decryption_pbkdf2_aes256gcm(enc_key_pair: Vec<u8>, password: String) -> String {
    const CREDENTIAL_LEN: usize = 32;
    const IV_LEN: usize = 12;
    let n_iter = NonZeroU32::new(32).unwrap();

    if enc_key_pair.len() <= CREDENTIAL_LEN + IV_LEN {
        return "".to_string();
    }

    let salt = &enc_key_pair[0..CREDENTIAL_LEN];
    let iv = &enc_key_pair[CREDENTIAL_LEN..(CREDENTIAL_LEN + IV_LEN)];
    let ciphertext = &enc_key_pair[(CREDENTIAL_LEN + IV_LEN)..];

    let mut derived_key = [0u8; CREDENTIAL_LEN];
    pbkdf2::derive(
        pbkdf2::PBKDF2_HMAC_SHA512,
        n_iter,
        salt,
        password.as_bytes(),
        &mut derived_key,
    );
    let cipher = Aes256Gcm::new(GenericArray::from_slice(&derived_key));
    let plaintext = cipher
        .decrypt(GenericArray::from_slice(iv), ciphertext.as_ref())
        .unwrap_or_default();

    String::from_utf8(plaintext).unwrap_or_else(|_| "".to_string())
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
pub fn create_keypair_from_secret(sk_str: String) -> Option<XfrKeyPair> {
    serde_json::from_str::<XfrSecretKey>(&sk_str)
        .map(|sk| sk.into_keypair())
        .ok()
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
pub fn get_pk_from_keypair(kp: &XfrKeyPair) -> XfrPublicKey {
    kp.get_pk()
}

/// Randomly generate a 12words-length mnemonic.
#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
pub fn generate_mnemonic_default() -> String {
    wallet::generate_mnemonic_default()
}

/// Generate mnemonic with custom length and language.
/// - @param `wordslen`: acceptable value are one of [ 12, 15, 18, 21, 24 ]
/// - @param `lang`: acceptable value are one of [ "en", "zh", "zh_traditional", "fr", "it", "ko", "sp", "jp" ]
pub fn rs_generate_mnemonic_custom(wordslen: u8, lang: &str) -> Result<String> {
    wallet::generate_mnemonic_custom(wordslen, lang)
}

/// Use this struct to express a Bip44/Bip49 path.
#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
pub struct BipPath {
    coin: u32,
    account: u32,
    change: u32,
    address: u32,
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
impl BipPath {
    pub fn new(coin: u32, account: u32, change: u32, address: u32) -> Self {
        BipPath {
            coin,
            account,
            change,
            address,
        }
    }
}

impl From<&BipPath> for wallet::BipPath {
    fn from(p: &BipPath) -> Self {
        wallet::BipPath::new(p.coin, p.account, p.change, p.address)
    }
}

/// Restore the XfrKeyPair from a mnemonic with a default bip44-path,
/// that is "m/44'/917'/0'/0/0" ("m/44'/coin'/account'/change/address").
pub fn rs_restore_keypair_from_mnemonic_default(phrase: &str) -> Result<XfrKeyPair> {
    wallet::restore_keypair_from_mnemonic_default(phrase)
}

/// Restore the XfrKeyPair from a mnemonic with custom params,
/// in bip44 form.
pub fn rs_restore_keypair_from_mnemonic_bip44(
    phrase: &str,
    lang: &str,
    path: &BipPath,
) -> Result<XfrKeyPair> {
    wallet::restore_keypair_from_mnemonic_bip44(phrase, lang, &path.into())
}

/// Restore the XfrKeyPair from a mnemonic with custom params,
/// in bip49 form.
pub fn rs_restore_keypair_from_mnemonic_bip49(
    phrase: &str,
    lang: &str,
    path: &BipPath,
) -> Result<XfrKeyPair> {
    wallet::restore_keypair_from_mnemonic_bip49(phrase, lang, &path.into())
}

/// ID of FRA, in `String` format.
#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
pub fn fra_get_asset_code() -> String {
    AssetTypeCode {
        val: ASSET_TYPE_FRA,
    }
    .to_base64()
}

/// Fee smaller than this value will be denied.
#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
pub fn fra_get_minimal_fee() -> u64 {
    TX_FEE_MIN
}

/// The destination for fee to be transfered to.
#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
pub fn fra_get_dest_pubkey() -> XfrPublicKey {
    *BLACK_HOLE_PUBKEY
}

/// The system address used to reveive delegation principals.
#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
pub fn get_delegation_target_address() -> String {
    get_coinbase_principal_address()
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
#[allow(missing_docs)]
pub fn get_coinbase_address() -> String {
    wallet::public_key_to_base64(&BLACK_HOLE_PUBKEY_STAKING)
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
#[allow(missing_docs)]
pub fn get_coinbase_principal_address() -> String {
    wallet::public_key_to_base64(&BLACK_HOLE_PUBKEY_STAKING)
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
#[allow(missing_docs)]
pub fn get_delegation_min_amount() -> u64 {
    MIN_DELEGATION_AMOUNT
}

#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
#[allow(missing_docs)]
pub fn get_delegation_max_amount() -> u64 {
    MAX_DELEGATION_AMOUNT
}
