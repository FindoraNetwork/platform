use std::fmt::Debug;

use jsonrpc_core::{Error, ErrorCode};
use tokio::task::JoinError;

pub fn convert_join_error_to_rpc_error(e: JoinError) -> Error {
    Error {
        code: ErrorCode::InternalError,
        message: format!("{:?}", e),
        data: None,
    }
}

pub fn convert_error_to_rpc_error(e: impl Debug) -> Error {
    Error {
        code: ErrorCode::InternalError,
        message: format!("{:?}", e),
        data: None,
    }
}

#[allow(dead_code)]
pub fn build_rpc_error() -> Error {
    Error::new(ErrorCode::InternalError)
}

// pub fn build_method_not_found() -> Error {
//     Error::method_not_found()
// }
