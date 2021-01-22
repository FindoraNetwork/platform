use bech32::{self, FromBase32, ToBase32};
use core::fmt::Display;
use ledger::data_model::b64dec;
use wasm_bindgen::prelude::*;
use zei::serialization::ZeiFromToBytes;
use zei::xfr::sig::XfrPublicKey;

pub(crate) fn error_to_jsvalue<T: Display>(e: T) -> JsValue {
    JsValue::from_str(&format!("{}", e))
}

pub(crate) fn public_key_from_base64(key_pair: String) -> Result<XfrPublicKey, JsValue> {
    XfrPublicKey::zei_from_bytes(&b64dec(&key_pair).map_err(error_to_jsvalue)?)
        .map_err(error_to_jsvalue)
}

pub(crate) fn public_key_from_bech32(key_pair: String) -> Result<XfrPublicKey, JsValue> {
    XfrPublicKey::zei_from_bytes(&bech32dec(&key_pair).map_err(error_to_jsvalue)?)
        .map_err(error_to_jsvalue)
}

pub fn bech32enc<T: AsRef<[u8]> + ToBase32>(input: &T) -> String {
    bech32::encode("fra", input.to_base32()).unwrap()
}

pub fn bech32dec(input: &str) -> Result<Vec<u8>, bech32::Error> {
    let res = bech32::decode(input);
    return match res {
        Ok((_hrp, data)) => Vec::<u8>::from_base32(&data),
        Err(e) => Err(e),
    };
}
