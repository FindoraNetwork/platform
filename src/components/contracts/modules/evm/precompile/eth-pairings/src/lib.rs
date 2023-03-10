#[cfg(test)]
mod tests;

use eth_pairings::public_interface::{perform_operation, ApiError, OperationType};
use evm::executor::stack::{PrecompileFailure, PrecompileOutput};
use evm::{Context, ExitError, ExitSucceed};
use evm_precompile_utils::{EvmDataReader, EvmDataWriter, EvmResult, Gasometer};
use module_evm::precompile::{FinState, Precompile, PrecompileId, PrecompileResult};
use tracing::debug;

/// The Verifier precompile.
pub struct EthPairing;

impl PrecompileId for EthPairing {
    fn contract_id() -> u64 {
        0x2001
    }
}

#[evm_precompile_utils::generate_function_selector]
#[derive(Debug, PartialEq, Eq, num_enum::TryFromPrimitive, num_enum::IntoPrimitive)]
pub enum Call {
    ExecuteOperation = "executeOperation()",
}

impl Precompile for EthPairing {
    fn execute(
        input: &[u8],
        target_gas: Option<u64>,
        _context: &Context,
        _state: &FinState,
    ) -> PrecompileResult {
        let mut input = EvmDataReader::new(input);
        let selector = match input.read_selector::<Call>() {
            Ok(v) => v,
            Err(e) => {
                return Err(PrecompileFailure::Error { exit_status: e });
            }
        };

        match &selector {
            Call::ExecuteOperation => match Self::execute_operation(input, target_gas) {
                Ok(v) => Ok(v),
                Err(e) => Err(PrecompileFailure::Error { exit_status: e }),
            },
        }
    }
}

impl EthPairing {
    fn get_operation(data: &[u8]) -> Result<(OperationType, &[u8]), ApiError> {
        if data.is_empty() {
            return Err(ApiError::InputError("input is zero length".to_owned()));
        }
        let op = OperationType::from_u8(data[0]).ok_or(ApiError::MissingValue)?;
        Ok((op, &data[1..]))
    }

    fn call_public_api_on_vector(data: &[u8]) -> Result<Vec<u8>, ApiError> {
        let (op, input) = Self::get_operation(data)?;
        perform_operation(op, input)
    }

    fn calculate_gas_for_operation(data: &[u8]) -> Result<u64, ApiError> {
        let (op, input) = Self::get_operation(data)?;
        eth_pairings::gas_meter::meter_operation(op, input)
    }

    fn execute_operation(
        mut input: EvmDataReader,
        target_gas: Option<u64>,
    ) -> EvmResult<PrecompileOutput> {
        // Calculate and record required gas for operation
        let mut gasometer = Gasometer::new(target_gas);
        let gas = match Self::calculate_gas_for_operation(input.get_slice()) {
            Ok(res) => res,
            Err(api_err) => return Err(ExitError::Other(api_err.to_string().into())),
        };
        gasometer.record_cost(gas)?;

        debug!(target: "evm", "EthPairing#executingOp");
        let result = match Self::call_public_api_on_vector(input.get_slice()) {
            Ok(res) => res,
            Err(api_err) => return Err(ExitError::Other(api_err.to_string().into())),
        };

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            cost: gasometer.used_gas(),
            output: EvmDataWriter::new()
                .write_raw_bytes(result.as_slice())
                .build(),
            logs: vec![],
        })
    }
}
