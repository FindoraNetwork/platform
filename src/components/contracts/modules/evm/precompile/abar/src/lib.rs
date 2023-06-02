use ethabi::{decode, encode, ParamType, Token};
use evm::executor::stack::{PrecompileFailure, PrecompileOutput};
use evm::{Context, ExitError, ExitSucceed};
use evm_abar::EvmToAbarNote;
use evm_precompile_utils::EvmDataReader;
use module_evm::precompile::{FinState, Precompile, PrecompileId, PrecompileResult};
use platform_lib_noah::{
    noah_algebra::serialization::NoahFromToBytes, noah_api::parameters::VerifierParams,
};

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

        // params: decode(address,uint256,bytes);
        let mut tokens = decode(
            &[ParamType::Address, ParamType::Uint(256), ParamType::Bytes],
            input,
        )
        .map_err(|_| PrecompileFailure::Error {
            exit_status: ExitError::Other("input invalid bytes".into()),
        })?;

        let proof_bytes = tokens.pop().unwrap().into_bytes().unwrap(); // safe
        let amount = tokens.pop().unwrap().into_uint().unwrap(); // safe
        let asset = tokens.pop().unwrap().into_address().unwrap(); // safe

        let note = EvmToAbarNote::from_bytes(&proof_bytes).map_err(|_| {
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

        // return: encode(bytes32, bytes);
        let comm = Token::FixedBytes(note.output.commitment.noah_to_bytes());
        let memo = Token::Bytes(note.memo.0);
        let bytes = encode(&[comm, memo]);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            cost: gas_cost,
            output: bytes,
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
