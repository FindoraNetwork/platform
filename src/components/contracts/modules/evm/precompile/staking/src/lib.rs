#[cfg(test)]
mod tests;

use core::marker::PhantomData;
use ethereum_types::H160;
use evm::{executor::PrecompileOutput, Context, ExitError, ExitSucceed};
use evm_precompile_utils::{
    Address, EvmDataReader, EvmDataWriter, EvmResult, Gasometer,
};
use fp_traits::evm::AddressMapping;
use module_evm::{
    precompile::{FinState, Precompile, PrecompileId},
    Config,
};

const FRA_PUBKEY: u64 = 0;

pub struct Staking<C> {
    _marker: PhantomData<C>,
}

impl<C: Config> PrecompileId for Staking<C> {
    fn contract_id() -> u64 {
        0x101
    }
}

#[evm_precompile_utils::generate_function_selector]
#[derive(Debug, PartialEq, num_enum::TryFromPrimitive, num_enum::IntoPrimitive)]
pub enum Call {
    FraPubkey = "fraPubkey(address)",
}

impl<C: Config> Precompile for Staking<C> {
    fn execute(
        input: &[u8],
        target_gas: Option<u64>,
        _context: &Context,
        state: &FinState,
    ) -> Result<PrecompileOutput, ExitError> {
        let mut input = EvmDataReader::new(input);

        match &input.read_selector()? {
            Call::FraPubkey => Self::fra_pubkey(state, input, target_gas),
        }
    }
}

impl<C: Config> Staking<C> {
    fn fra_pubkey(
        state: &FinState,
        mut input: EvmDataReader,
        target_gas: Option<u64>,
    ) -> EvmResult<PrecompileOutput> {
        let mut gasometer = Gasometer::new(target_gas);
        gasometer.record_cost(FRA_PUBKEY)?;

        input.expect_arguments(1)?;

        let evm_address: H160 = input.read::<Address>()?.into();

        let fra_pubkey = C::AddressMapping::fra_pubkey(state, &evm_address);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            cost: gasometer.used_gas(),
            output: EvmDataWriter::new().write(fra_pubkey).build(),
            logs: vec![],
        })
    }
}
