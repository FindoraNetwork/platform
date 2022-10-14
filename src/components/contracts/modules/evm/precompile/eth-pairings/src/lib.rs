#[cfg(test)]
mod tests;

use eth_pairings::public_interface::{perform_operation, ApiError, OperationType};
use evm::executor::stack::{PrecompileFailure, PrecompileOutput};
use evm::{Context, ExitError, ExitSucceed};
use evm_precompile_utils::{EvmDataReader, EvmDataWriter, EvmResult, Gasometer};
use log::debug;
use module_evm::precompile::{FinState, Precompile, PrecompileId, PrecompileResult};

// The gas used value is obtained according to the standard erc20 call.
// https://github.com/OpenZeppelin/openzeppelin-contracts/blob/v4.3.2/contracts/token/ERC20/ERC20.sol
//const GAS_NAME: u64 = 3283;
const GAS_SYMBOL: u64 = 3437;
// const GAS_DECIMALS: u64 = 243;
// const GAS_TOTAL_SUPPLY: u64 = 1003;
// const GAS_BALANCE_OF: u64 = 1350;
// const GAS_TRANSFER: u64 = 23661;
// const GAS_ALLOWANCE: u64 = 1624;
// const GAS_APPROVE: u64 = 20750;
// const GAS_TRANSFER_FROM: u64 = 6610;

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
    fn call_public_api_on_vector(data: &[u8]) -> Result<Vec<u8>, ApiError> {
        if data.len() == 0 {
            return Err(ApiError::InputError("input is zero length".to_owned()));
        }
        let op = OperationType::from_u8(data[0]).ok_or(ApiError::MissingValue)?;
        perform_operation(op, &data[0..])
    }

    fn execute_operation(
        mut input: EvmDataReader,
        target_gas: Option<u64>,
    ) -> EvmResult<PrecompileOutput> {
        let mut gasometer = Gasometer::new(target_gas);
        gasometer.record_cost(GAS_SYMBOL)?;

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
