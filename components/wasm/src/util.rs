use wasm_bindgen::prelude::*;

// use core::fmt::Display;
//
// #[inline(always)]
// pub(crate) fn error_to_jsvalue<T: Display>(e: T) -> JsValue {
//     JsValue::from_str(&format!("{}", e))
// }

#[inline(always)]
pub(crate) fn ruc_to_jsvalue<T: AsRef<dyn ruc::err::RucError>>(e: T) -> JsValue {
    JsValue::from_str(&format!("{}", ruc::genlog(e.as_ref())))
}
