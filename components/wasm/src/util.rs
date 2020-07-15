use core::fmt::Display;
use ledger::data_model::b64dec;
use wasm_bindgen::prelude::*;
use zei::serialization::ZeiFromToBytes;
use zei::xfr::sig::XfrPublicKey;

pub(crate) fn error_to_jsvalue<T: Display>(e: T) -> JsValue {
  JsValue::from_str(&format!("{}", e))
}

pub(crate) fn public_key_from_base64(key_pair: String) -> Result<XfrPublicKey, JsValue> {
  XfrPublicKey::zei_from_bytes(&b64dec(&key_pair).map_err(error_to_jsvalue)?).map_err(error_to_jsvalue)
}
