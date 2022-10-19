use crate::rust::types;
use jni::objects::JClass;
use jni::sys::{jbyteArray, jlong};
use jni::JNIEnv;
use noah::xfr::sig::XfrKeyPair as RawXfrKeyPair;
use noah::xfr::structs::ASSET_TYPE_LENGTH;
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;

#[no_mangle]
pub unsafe extern "system" fn Java_com_findora_JniApi_xfrKeyPairNew(
    env: JNIEnv,
    _class: JClass,
    seed: jbyteArray,
) -> jlong {
    let input = env.convert_byte_array(seed).unwrap();
    let mut buf = [0u8; ASSET_TYPE_LENGTH];
    buf.copy_from_slice(input.as_ref());
    let mut prng = ChaChaRng::from_seed(buf);
    let val = types::XfrKeyPair::from(RawXfrKeyPair::generate(&mut prng));
    Box::into_raw(Box::new(val)) as jlong
}

#[no_mangle]
pub unsafe extern "system" fn Java_com_findora_JniApi_xfrKeyPairDestroy(
    _env: JNIEnv,
    _class: JClass,
    xfr_keypair_ptr: jlong,
) {
    let _boxed_key = Box::from_raw(xfr_keypair_ptr as *mut types::XfrKeyPair);
}
