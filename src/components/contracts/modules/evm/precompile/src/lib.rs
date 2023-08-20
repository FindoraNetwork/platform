use ethereum_types::H160;
use evm::{executor::stack::PrecompileSet, Context};
use module_evm::precompile::{Precompile, PrecompileResult};
use std::marker::PhantomData;

use evm_precompile_anemoi::{Anemoi254, Anemoi381};
use evm_precompile_basic::{ECRecover, Identity, Ripemd160, Sha256};
use evm_precompile_blake2::Blake2F;
use evm_precompile_bn128::{Bn128Add, Bn128Mul, Bn128Pairing};
use evm_precompile_frc20::FRC20;
use evm_precompile_modexp::Modexp;
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
        std::vec![0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0x1000, 0x2001, 0x2002]
            .into_iter()
            .map(hash)
            .collect()
    }
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
        let ctx = &self.1;

        match address {
            // Ethereum precompiles :
            a if a == H160::from_low_u64_be(ECRecover::contract_id()) => {
                Some(ECRecover::execute(input, target_gas, context, ctx))
            }
            a if a == H160::from_low_u64_be(Sha256::contract_id()) => {
                Some(Sha256::execute(input, target_gas, context, ctx))
            }
            a if a == H160::from_low_u64_be(Ripemd160::contract_id()) => {
                Some(Ripemd160::execute(input, target_gas, context, ctx))
            }
            a if a == H160::from_low_u64_be(Identity::contract_id()) => {
                Some(Identity::execute(input, target_gas, context, ctx))
            }
            a if a == H160::from_low_u64_be(Modexp::contract_id()) => {
                Some(Modexp::execute(input, target_gas, context, ctx))
            }
            a if a == H160::from_low_u64_be(Bn128Add::contract_id()) => {
                Some(Bn128Add::execute(input, target_gas, context, ctx))
            }
            a if a == H160::from_low_u64_be(Bn128Mul::contract_id()) => {
                Some(Bn128Mul::execute(input, target_gas, context, ctx))
            }
            a if a == H160::from_low_u64_be(Bn128Pairing::contract_id()) => {
                Some(Bn128Pairing::execute(input, target_gas, context, ctx))
            }
            a if a == H160::from_low_u64_be(Blake2F::contract_id()) => {
                Some(Blake2F::execute(input, target_gas, context, ctx))
            }
            a if a == H160::from_low_u64_be(FRC20::<C>::contract_id()) => {
                Some(FRC20::<C>::execute(input, target_gas, context, ctx))
            }
            a if a == H160::from_low_u64_be(Anemoi381::contract_id()) => {
                Some(Anemoi381::execute(input, target_gas, context, ctx))
            }
            a if a == H160::from_low_u64_be(Anemoi254::contract_id()) => {
                Some(Anemoi254::execute(input, target_gas, context, ctx))
            }
            //a if a == H160::from_low_u64_be(EthPairing::contract_id()) => {
            //    Some(EthPairing::execute(input, target_gas, context, ctx))
            //}
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
