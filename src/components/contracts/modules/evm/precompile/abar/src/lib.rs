use ethereum_types::{Address, U256};
use evm::executor::stack::{PrecompileFailure, PrecompileOutput};
use evm::{Context, ExitError, ExitSucceed};
use evm_abar::EvmToAbarNote;
use evm_precompile_utils::EvmDataReader;
use module_evm::precompile::{FinState, Precompile, PrecompileId, PrecompileResult};
use platform_lib_noah::noah_api::parameters::VerifierParams;

/// The Abar precompile
pub struct Abar;

impl Abar {
    pub const GAS_PER_PERM: u64 = 125000;

    pub fn evm_to_abar(input: &[u8], target_gas: Option<u64>) -> PrecompileResult {
        let gas_cost = 4 * Self::GAS_PER_PERM;
        if let Some(gas_left) = target_gas {
            if gas_left < gas_cost {
                return Err(PrecompileFailure::Error {
                    exit_status: ExitError::OutOfGas,
                });
            }
        };

        // 20 + 32 = 52
        if input.len() < 52 {
            return Err(PrecompileFailure::Error {
                exit_status: ExitError::Other("input must more than 52 bytes".into()),
            });
        }
        let asset = Address::from_slice(&input[0..20]);
        let amount = U256::from_big_endian(&input[20..52]);
        let note = EvmToAbarNote::from_bytes(&input[52..]).map_err(|_| {
            PrecompileFailure::Error {
                exit_status: ExitError::Other("deserialize failure".into()),
            }
        })?;

        let verifier_params =
            VerifierParams::get_ar_to_abar().map_err(|_| PrecompileFailure::Error {
                exit_status: ExitError::Other("params failure".into()),
            })?;

        note.verify(&verifier_params, asset, amount).map_err(|_| {
            PrecompileFailure::Error {
                exit_status: ExitError::Other("verify failure".into()),
            }
        })?;

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            cost: gas_cost,
            output: note.returns(),
            logs: Default::default(),
        })
    }

    pub fn abar_to_evm(_input: &[u8], target_gas: Option<u64>) -> PrecompileResult {
        let gas_cost = 4 * Self::GAS_PER_PERM;
        if let Some(gas_left) = target_gas {
            if gas_left < gas_cost {
                return Err(PrecompileFailure::Error {
                    exit_status: ExitError::OutOfGas,
                });
            }
        };

        let res = vec![];

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            cost: gas_cost,
            output: res,
            logs: Default::default(),
        })
    }

    pub fn abar_to_abar(_input: &[u8], target_gas: Option<u64>) -> PrecompileResult {
        let gas_cost = 4 * Self::GAS_PER_PERM;
        if let Some(gas_left) = target_gas {
            if gas_left < gas_cost {
                return Err(PrecompileFailure::Error {
                    exit_status: ExitError::OutOfGas,
                });
            }
        };

        let res = vec![];

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            cost: gas_cost,
            output: res,
            logs: Default::default(),
        })
    }
}

#[evm_precompile_utils::generate_function_selector]
#[derive(Debug, PartialEq, Eq, num_enum::TryFromPrimitive, num_enum::IntoPrimitive)]
pub enum Call {
    EvmToAbar = "evmToAbar(address,uint256,bytes)",
    AbarToEvm = "abarToEvm(address,uint256,bytes32,bytes32,bytes)",
    AbarToAbar = "abarToAbar(bytes32,bytes32,bytes)",
}

impl PrecompileId for Abar {
    fn contract_id() -> u64 {
        0x2003
    }
}

impl Precompile for Abar {
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
            Call::EvmToAbar => Self::evm_to_abar(input.get_slice(), target_gas),
            Call::AbarToEvm => Self::abar_to_evm(input.get_slice(), target_gas),
            Call::AbarToAbar => Self::abar_to_abar(input.get_slice(), target_gas),
        }
    }
}
