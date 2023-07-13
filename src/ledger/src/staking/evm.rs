//! For interact with BaseApp (EVM)

use super::{Delegation, Validator};
use fp_types::H160;
use once_cell::sync::{Lazy, OnceCell};
use parking_lot::{Mutex, RwLock};
use ruc::Result;
use std::{collections::BTreeMap, sync::Arc};
use zei::xfr::sig::XfrPublicKey;

///EVM staking interface
pub static EVM_STAKING: OnceCell<Arc<RwLock<dyn EVMStaking>>> = OnceCell::new();

///Mints from EVM staking
pub static EVM_STAKING_MINTS: Lazy<Mutex<Vec<(XfrPublicKey, u64)>>> =
    Lazy::new(|| Mutex::new(Vec::with_capacity(64)));

/// For account base app
pub trait EVMStaking: Sync + Send + 'static {
    /// import_validators call
    fn import_validators(
        &self,
        validators: &[Validator],
        delegations: &BTreeMap<XfrPublicKey, Delegation>,
        coinbase_balance: u64,
    ) -> Result<()>;
    /// stake call
    fn stake(
        &self,
        from: &XfrPublicKey,
        value: u64,
        td_addr: &[u8],
        td_pubkey: Vec<u8>,
        memo: String,
        rate: [u64; 2],
    ) -> Result<()>;
    /// delegate call
    fn delegate(&self, from: &XfrPublicKey, value: u64, td_addr: &[u8]) -> Result<()>;
    /// undelegate call
    fn undelegate(&self, from: &XfrPublicKey, td_addr: &[u8], amount: u64)
        -> Result<()>;
    ///update the memo and rate of the validator
    fn update_validator(
        &self,
        staker: &XfrPublicKey,
        validator: &[u8],
        memo: String,
        rate: [u64; 2],
    ) -> Result<()>;
    ///
    fn replace_delegator(
        &self,
        validator: &[u8],
        staker: &XfrPublicKey,
        new_staker_address: H160,
    ) -> Result<()>;
    /// claim call
    fn claim(&self, td_addr: &[u8], delegator_pk: &XfrPublicKey) -> Result<()>;
}
