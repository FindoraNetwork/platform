#![deny(warnings)]
#![allow(missing_docs)]

mod eth;
mod eth_filter;
mod eth_pubsub;
mod net;
mod web3;

use baseapp::BaseApp;
use evm::{ExitError, ExitReason};
use fp_rpc_core::types::pubsub::Metadata;
use fp_rpc_core::{
    EthApiServer, EthFilterApiServer, EthPubSubApiServer, NetApiServer, Web3ApiServer,
};
use fp_rpc_server::{rpc_handler, start_http, start_ws, RpcHandler, RpcMiddleware};
use fp_utils::ecdsa::SecpPair;
use jsonrpc_core::*;
use log::error;
use parking_lot::RwLock;
use rustc_hex::ToHex;
use serde_json::Value;
use std::sync::Arc;

pub fn start_web3_service(
    evm_http: String,
    evm_ws: String,
    tendermint_rpc: String,
    account_base_app: Arc<RwLock<BaseApp>>,
) -> Box<dyn std::any::Any + Send> {
    // PrivateKey: 9f7bebaa5c55464b10150bc2e0fd552e915e2bdbca95cc45ed1c909aca96e7f5
    // Address: 0xf6aca39539374993b37d29ccf0d93fa214ea0af1
    let dev_signer = "zebra paddle unveil toilet weekend space gorilla lesson relief useless arrive picture";
    let signers = vec![SecpPair::from_phrase(dev_signer, None).unwrap().0];

    let io = || -> RpcHandler<Metadata> {
        rpc_handler(
            (
                eth::EthApiImpl::new(
                    tendermint_rpc.clone(),
                    account_base_app.clone(),
                    signers.clone(),
                )
                .to_delegate(),
                eth_filter::EthFilterApiImpl::new().to_delegate(),
                net::NetApiImpl::new().to_delegate(),
                web3::Web3ApiImpl::new().to_delegate(),
                eth_pubsub::EthPubSubApiImpl::new(account_base_app.clone())
                    .to_delegate(),
            ),
            RpcMiddleware::new(),
        )
    };

    let http_server = start_http(
        &evm_http.parse().unwrap(),
        None,
        Some(&vec!["*".to_string()]),
        io(),
        None,
    )
    .map(|s| waiting::HttpServer(Some(s)))
    .expect("Unable to start web3 http service");

    let ws_server = start_ws(
        &evm_ws.parse().unwrap(),
        None,
        Some(&vec!["*".to_string()]),
        io(),
        None,
    )
    .map(|s| waiting::WsServer(Some(s)))
    .expect("Unable to start web3 ws service");

    Box::new((http_server, ws_server))
}

// Wrapper for HTTP and WS servers that makes sure they are properly shut down.
mod waiting {
    pub struct HttpServer(pub Option<fp_rpc_server::HttpServer>);
    impl Drop for HttpServer {
        fn drop(&mut self) {
            if let Some(server) = self.0.take() {
                server.close_handle().close();
                server.wait();
            }
        }
    }

    pub struct IpcServer(pub Option<fp_rpc_server::IpcServer>);
    impl Drop for IpcServer {
        fn drop(&mut self) {
            if let Some(server) = self.0.take() {
                server.close_handle().close();
                let _ = server.wait();
            }
        }
    }

    pub struct WsServer(pub Option<fp_rpc_server::WsServer>);
    impl Drop for WsServer {
        fn drop(&mut self) {
            if let Some(server) = self.0.take() {
                server.close_handle().close();
                let _ = server.wait();
            }
        }
    }
}

pub fn internal_err<T: ToString>(message: T) -> Error {
    error!(target: "eth_rpc", "internal error: {:?}", message.to_string());
    Error {
        code: ErrorCode::InternalError,
        message: message.to_string(),
        data: None,
    }
}

pub fn error_on_execution_failure(
    reason: &ExitReason,
    data: &[u8],
) -> std::result::Result<(), Error> {
    match reason {
        ExitReason::Succeed(_) => Ok(()),
        ExitReason::Error(e) => {
            if *e == ExitError::OutOfGas {
                // `ServerError(0)` will be useful in estimate gas
                return Err(Error {
                    code: ErrorCode::ServerError(0),
                    message: "out of gas".to_string(),
                    data: None,
                });
            }
            Err(Error {
                code: ErrorCode::InternalError,
                message: format!("evm error: {:?}", e),
                data: Some(Value::String("0x".to_string())),
            })
        }
        ExitReason::Revert(_) => {
            let mut message =
                "VM Exception while processing transaction: revert".to_string();
            // A minimum size of error function selector (4) + offset (32) + string length (32)
            // should contain a utf-8 encoded revert reason.
            if data.len() > 68 {
                let message_len = data[36..68].iter().sum::<u8>();
                let body: &[u8] = &data[68..68 + message_len as usize];
                if let Ok(reason) = std::str::from_utf8(body) {
                    message = format!("{} {}", message, reason.to_string());
                }
            }
            Err(Error {
                code: ErrorCode::InternalError,
                message,
                data: Some(Value::String(data.to_hex())),
            })
        }
        ExitReason::Fatal(e) => Err(Error {
            code: ErrorCode::InternalError,
            message: format!("evm fatal: {:?}", e),
            data: Some(Value::String("0x".to_string())),
        }),
    }
}
