//! For interact with BaseApp (EVM)

use ruc::Result;

use once_cell::sync::OnceCell;
use parking_lot::RwLock;
use std::sync::Arc;

use noah::xfr::sig::XfrPublicKey;

///EVM staking interface
pub static EVM_STAKING: OnceCell<Arc<RwLock<dyn EVMStaking>>> = OnceCell::new();

/// For account base app
pub trait EVMStaking: Sync + Send + 'static {
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
    /// claim call
    fn claim(&self, from: &XfrPublicKey, amount: u64) -> Result<()>;
}
