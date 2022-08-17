#![deny(warnings)]
#![allow(missing_docs)]

mod basic;
mod impls;

#[cfg(test)]
mod tests;

use abci::{RequestQuery, ResponseQuery};
use fp_core::{context::Context, module::AppModule};
use fp_traits::{
    account::{AccountAsset, FeeCalculator},
    evm::{DecimalsMapping, EthereumDecimalsMapping},
};
use fp_types::crypto::Address;
use std::marker::PhantomData;

pub const MODULE_NAME: &str = "account";

pub trait Config {
    type FeeCalculator: FeeCalculator;
}

impl Config for () {
    type FeeCalculator = ();
}

mod storage {
    use fp_core::account::SmartAccount;
    use fp_types::crypto::Address;
    use primitive_types::{H160, U256};
    use zei::xfr::sig::XfrPublicKey;

    use fp_storage::*;

    // Store account information under all account addresses
    generate_storage!(Account, AccountStore => Map<Address, SmartAccount>);
    // The total units transferred from the fra UTXO side.
    generate_storage!(Account, TotalIssuance => Value<U256>);
    // The owner approve his amount of funds to the spender.
    // owner => spender => amount
    generate_storage!(Account, Allowances => DoubleMap<Address, Address, U256>);

    // evm_address => fra_pk
    generate_storage!(Account, EvmFraAddressMapping => Map<H160, XfrPublicKey>);
}

#[derive(Clone)]
pub struct App<C> {
    phantom: PhantomData<C>,
}

impl<C: Config> Default for App<C> {
    fn default() -> Self {
        App {
            phantom: Default::default(),
        }
    }
}

impl<C: Config> AppModule for App<C> {
    fn query_route(
        &self,
        ctx: Context,
        path: Vec<&str>,
        req: &RequestQuery,
    ) -> ResponseQuery {
        let mut resp: ResponseQuery = Default::default();
        if path.len() != 1 {
            resp.code = 1;
            resp.log = String::from("account: invalid query path");
            return resp;
        }
        match path[0] {
            "info" => {
                let data = serde_json::from_slice::<Address>(req.data.as_slice());
                if data.is_err() {
                    resp.code = 1;
                    resp.log = String::from("account: query nonce with invalid params");
                    return resp;
                }
                let mut info =
                    Self::account_of(&ctx, &data.unwrap(), None).unwrap_or_default();
                info.balance =
                    EthereumDecimalsMapping::convert_to_native_token(info.balance);
                info.reserved =
                    EthereumDecimalsMapping::convert_to_native_token(info.reserved);

                if let Ok(value) = serde_json::to_vec(&info) {
                    resp.value = value;
                } else {
                    resp.code = 1;
                    resp.log = String::from("account: failed to serialize account data");
                }

                resp
            }
            "nonce" => {
                let data = serde_json::from_slice::<Address>(req.data.as_slice());
                if data.is_err() {
                    resp.code = 1;
                    resp.log = String::from("account: query nonce with invalid params");
                    return resp;
                }
                let nonce = Self::nonce(&ctx, &data.unwrap());

                if let Ok(value) = serde_json::to_vec(&nonce) {
                    resp.value = value;
                } else {
                    resp.code = 1;
                    resp.log = String::from("account: failed to serialize account data");
                }
                resp
            }
            _ => resp,
        }
    }
}
