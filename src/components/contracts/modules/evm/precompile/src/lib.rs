use ethereum_types::H160;
use evm::executor::stack::{PrecompileHandle, PrecompileSet};
// use evm::Context;
use module_evm::precompile::{Precompile, PrecompileResult};
use std::marker::PhantomData;

use evm_precompile_basic::{ECRecover, ECRecoverPublicKey, Identity, Ripemd160, Sha256};
use evm_precompile_frc20::FRC20;
use evm_precompile_modexp::Modexp;
use evm_precompile_sha3fips::{Sha3FIPS256, Sha3FIPS512};
use fp_core::context::Context as Context2;
use module_evm::precompile::PrecompileId;
use module_evm::Config;

pub struct FindoraPrecompiles<R>(PhantomData<R>, Context2);

impl<R> FindoraPrecompiles<R>
where
    R: Config,
{
    pub fn new(ctx: Context2) -> Self {
        Self(Default::default(), ctx)
    }
    pub fn used_addresses() -> std::vec::Vec<H160> {
        std::vec![1, 2, 3, 4, 5, 1024, 1025]
            .into_iter()
            .map(hash)
            .collect()
    }
}

impl<C> PrecompileSet for FindoraPrecompiles<C>
where
    C: Config,
{
    fn execute(&self, handle: &mut impl PrecompileHandle) -> Option<PrecompileResult> {
        let ctx = &self.1;

        match handle.code_address() {
            // Ethereum precompiles :
            a if a == H160::from_low_u64_be(ECRecover::contract_id()) => {
                Some(ECRecover::execute(handle, ctx))
            }
            a if a == H160::from_low_u64_be(Sha256::contract_id()) => {
                Some(Sha256::execute(handle, ctx))
            }
            a if a == H160::from_low_u64_be(Ripemd160::contract_id()) => {
                Some(Ripemd160::execute(handle, ctx))
            }
            a if a == H160::from_low_u64_be(Identity::contract_id()) => {
                Some(Identity::execute(handle, ctx))
            }
            a if a == H160::from_low_u64_be(Modexp::contract_id()) => {
                Some(Modexp::execute(handle, ctx))
            }
            // Non-Frontier specific nor Ethereum precompiles :
            a if a == H160::from_low_u64_be(ECRecoverPublicKey::contract_id()) => {
                Some(ECRecoverPublicKey::execute(handle, ctx))
            }
            a if a == H160::from_low_u64_be(Sha3FIPS256::contract_id()) => {
                Some(Sha3FIPS256::execute(handle, ctx))
            }
            a if a == H160::from_low_u64_be(Sha3FIPS512::contract_id()) => {
                Some(Sha3FIPS512::execute(handle, ctx))
            }
            a if a == H160::from_low_u64_be(FRC20::<C>::contract_id()) => {
                Some(FRC20::<C>::execute(handle, ctx))
            }
            // a if a == H160::from_low_u64_be(EthPairing::contract_id()) => {
            //     Some(EthPairing::execute(handle, ctx))
            // }
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
