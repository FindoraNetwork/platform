#[cfg(target_arch = "wasm32")]
use core::fmt::Display;
#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

use std::ffi::{CStr, CString};
use std::os::raw::c_char;

#[allow(clippy::not_unsafe_ptr_arg_deref)]
#[inline(always)]
pub fn c_char_to_string(cchar: *const c_char) -> String {
    let c_str = unsafe { CStr::from_ptr(cchar) };
    c_str.to_str().unwrap_or("").to_string()
}

#[inline(always)]
pub fn string_to_c_char(r_string: String) -> *mut c_char {
    let c_str = CString::new(r_string).expect("CString::new failed");
    c_str.into_raw()
}

#[cfg(target_arch = "wasm32")]
#[inline(always)]
pub fn error_to_jsvalue<T: Display>(e: T) -> JsVal {
    JsValue::from_str(&e.to_string())
}
