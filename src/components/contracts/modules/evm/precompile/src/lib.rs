use ethereum_types::H160;
use evm::executor::stack::PrecompileSet;
use evm::Context;
use module_evm::precompile::{Precompile, PrecompileResult};
use std::{env::temp_dir, marker::PhantomData, path::PathBuf, time::SystemTime};

use evm_precompile_basic::{ECRecover, ECRecoverPublicKey, Identity, Ripemd160, Sha256};
use evm_precompile_modexp::Modexp;
use evm_precompile_sha3fips::{Sha3FIPS256, Sha3FIPS512};
use evm_precompile_frc20::FRC20;
use fin_db::{FinDB, RocksDB};
use fp_core::context::Context as Context2;
use module_evm::Config;
use parking_lot::RwLock;
use std::sync::Arc;
use storage::state::ChainState;
use module_evm::precompile::PrecompileId;

pub struct FindoraPrecompiles<R>(PhantomData<R>);

impl<R> FindoraPrecompiles<R>
where
    R: Config,
{
    pub fn new() -> Self {
        Self(Default::default())
    }
    pub fn used_addresses() -> std::vec::Vec<H160> {
        std::vec![1, 2, 3, 4, 5, 1024, 1025]
            .into_iter()
            .map(|x| hash(x))
            .collect()
    }
}

pub fn create_temp_db_path() -> PathBuf {
    let time = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    let mut path = temp_dir();
    path.push(format!("temp-findora-db–{}", time));
    path
}


fn create_temp_context2() -> Context2
{
    let basedir = create_temp_db_path();
    let fdb_path = basedir.join("state.db");
    let fdb = FinDB::open(fdb_path).unwrap();
    let chain_state = Arc::new(RwLock::new(ChainState::new(
        fdb,
        "findora_db".to_owned(),
        4 * 60 * 24 * 90,
    )));

    let rdb_path = basedir.join("history.db");
    let rdb = RocksDB::open(rdb_path.as_path()).unwrap();
    let chain_db =
        Arc::new(RwLock::new(ChainState::new(rdb, "rocks_db".to_owned(), 0)));

    Context2::new(chain_state, chain_db)
}

impl<C> PrecompileSet for FindoraPrecompiles<C>
where
    C: Config,
{
    fn execute(
        &self,
        address: H160,
        input: &[u8],
        target_gas: Option<u64>,
        context: &Context,
        _is_static: bool,
    ) -> Option<PrecompileResult> {
        let eliver_state_temp = create_temp_context2();

        match address {
            // Ethereum precompiles :
            a if a == H160::from_low_u64_be(ECRecover::contract_id()) => Some(
                ECRecover::execute(input, target_gas, context, &eliver_state_temp),
            ),
            a if a == H160::from_low_u64_be(Sha256::contract_id()) => {
                Some(Sha256::execute(input, target_gas, context, &eliver_state_temp))
            }
            a if a == H160::from_low_u64_be(Ripemd160::contract_id()) => Some(
                Ripemd160::execute(input, target_gas, context, &eliver_state_temp),
            ),
            a if a == H160::from_low_u64_be(Identity::contract_id()) => Some(
                Identity::execute(input, target_gas, context, &eliver_state_temp),
            ),
            a if a == H160::from_low_u64_be(Modexp::contract_id()) => {
                Some(Modexp::execute(input, target_gas, context, &eliver_state_temp))
            }
            // Non-Frontier specific nor Ethereum precompiles :
            a if a == H160::from_low_u64_be(ECRecoverPublicKey::contract_id()) => Some(
                ECRecoverPublicKey::execute(input, target_gas, context, &eliver_state_temp),
            ),
            a if a == H160::from_low_u64_be(Sha3FIPS256::contract_id()) => Some(
                Sha3FIPS256::execute(input, target_gas, context, &eliver_state_temp),
            ),
            a if a == H160::from_low_u64_be(Sha3FIPS512::contract_id()) => Some(
                Sha3FIPS512::execute(input, target_gas, context, &eliver_state_temp),
            ),
            a if a == H160::from_low_u64_be(FRC20::<C>::contract_id()) => Some(
                FRC20::<C>::execute(input, target_gas, context, &eliver_state_temp),
            ),
            _ => None,
        }
    }

    fn is_precompile(&self, address: H160) -> bool {
        Self::used_addresses().contains(&address)
    }
}

fn hash(a: u64) -> H160 {
    H160::from_low_u64_be(a)
}
