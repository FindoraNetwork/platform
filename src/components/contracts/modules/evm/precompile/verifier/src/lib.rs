use evm::{Context, ExitSucceed};
use evm_precompile_utils::{EvmDataReader, EvmDataWriter, EvmResult, Gasometer};
//use evm_precompile_utils::data::IndVerifierKey;
use evm::executor::stack::{PrecompileFailure, PrecompileOutput};
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
pub struct Verifier;

impl PrecompileId for Verifier {
    fn contract_id() -> u64 {
        0x2000
    }
}

#[evm_precompile_utils::generate_function_selector]
#[derive(Debug, PartialEq, Eq, num_enum::TryFromPrimitive, num_enum::IntoPrimitive)]
pub enum Call {
    Verify = "verify()",
    SupportsInterface = "supportsInterface()",
}

impl Precompile for Verifier {
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

        match selector {
            Call::Verify => match Self::verify(input, target_gas) {
                Ok(v) => Ok(v),
                Err(e) => Err(PrecompileFailure::Error { exit_status: e }),
            },
            Call::SupportsInterface => match Self::supports_interface() {
                Ok(v) => Ok(v),
                Err(e) => Err(PrecompileFailure::Error { exit_status: e }),
            },
        }
    }
}

impl Verifier {
    fn verify(
        _input: EvmDataReader,
        target_gas: Option<u64>,
    ) -> EvmResult<PrecompileOutput> {
        let mut gasometer = Gasometer::new(target_gas);
        gasometer.record_cost(GAS_SYMBOL)?;

        debug!(target: "evm", "Verifier#verify");
        //
        // let _ivk: IndVerifierKey = reader
        // .read()
        // .expect("to correctly parse IndexVerifierKey<Fr, MultiPC>");

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            cost: gasometer.used_gas(),
            output: EvmDataWriter::new().write_raw_bytes("T".as_bytes()).build(),
            logs: vec![],
        })
    }

    fn supports_interface() -> EvmResult<PrecompileOutput> {
        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            cost: 0,
            output: vec![],
            logs: vec![],
        })
    }
}
