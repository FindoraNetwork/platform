#![allow(clippy::needless_return)]

use core::fmt::Debug;
use jni::sys::jobject;
use jni::JNIEnv;

/// throw exception and return null if res is an Err(..), this behave like `?` but return null pointer.
macro_rules! throw_exception {
    ($env:expr, $res: expr) => {
        match ThrowExceptionImpl($env, $res) {
            Ok(t) => t,
            Err(null) => return null as _,
        }
    };
}

///Throw exception if result it's an Err(..) and return Err(null).
pub(super) fn ThrowExceptionImpl<T, E>(
    env: JNIEnv,
    result: Result<T, E>,
) -> Result<T, jobject>
where
    E: Debug,
{
    match result {
        Ok(t) => Ok(t),
        Err(e) => {
            let null = core::ptr::null_mut() as jobject;
            let exception_occurred = env.exception_occurred().unwrap();
            if !exception_occurred.is_null() {
                env.exception_describe().unwrap();
                return Err(null);
            }
            env.throw_new("java/lang/Exception", format!("{:?}", e))
                .unwrap();
            return Err(null);
        }
    }
}
