use ethereum_types::U256;
use evm::executor::stack::{PrecompileFailure, PrecompileOutput};
use evm::{Context, ExitError, ExitSucceed};
use evm_precompile_utils::EvmDataReader;
use module_evm::precompile::{FinState, Precompile, PrecompileId, PrecompileResult};
use platform_lib_noah::noah_algebra::bls12_381::BLSScalar;
use platform_lib_noah::noah_algebra::prelude::Scalar;
use platform_lib_noah::noah_crypto::basic::anemoi_jive::{
    AnemoiJive, AnemoiJive381, ANEMOI_JIVE_381_SALTS,
};

/// The Anemoi precompile
pub struct Anemoi;

impl Anemoi {
    pub const GAS_PER_PERM: u64 = 125000;

    pub fn eval_jive(input: &[u8], target_gas: Option<u64>) -> PrecompileResult {
        let gas_cost = 2 * Self::GAS_PER_PERM;
        if let Some(gas_left) = target_gas {
            if gas_left < gas_cost {
                return Err(PrecompileFailure::Error {
                    exit_status: ExitError::OutOfGas,
                });
            }
        };

        // 32 * 4 = 128
        if input.len() != 128 {
            return Err(PrecompileFailure::Error {
                exit_status: ExitError::Other("input must be 128 bytes".into()),
            });
        }

        let mut fields = Vec::with_capacity(3);

        for bytes in input[..96].chunks(32) {
            let le_bytes: Vec<u8> = bytes.iter().rev().copied().collect();
            if let Ok(res) = BLSScalar::from_bytes(&le_bytes) {
                fields.push(res);
            } else {
                return Err(PrecompileFailure::Error {
                    exit_status: ExitError::Other(
                        "Cannot convert bytes to field elements".into(),
                    ),
                });
            }
        }
        let index = U256::from_big_endian(&input[96..]);

        let mut res = AnemoiJive381::eval_jive(
            &[fields[0], fields[1]],
            &[fields[2], ANEMOI_JIVE_381_SALTS[index.as_usize()]],
        )
        .to_bytes();
        res.reverse();

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            cost: gas_cost,
            output: res,
            logs: Default::default(),
        })
    }

    pub fn eval_variable_length_hash(
        input: &[u8],
        target_gas: Option<u64>,
    ) -> PrecompileResult {
        if input.len() % 32 != 0 {
            return Err(PrecompileFailure::Error {
                exit_status: ExitError::Other(
                    "input must be multiplies of 32 bytes".into(),
                ),
            });
        }

        let num_elems = input.len() / 32;

        let gas_cost = ((num_elems + 2) / 3) as u64 * Self::GAS_PER_PERM;
        if let Some(gas_left) = target_gas {
            if gas_left < gas_cost {
                return Err(PrecompileFailure::Error {
                    exit_status: ExitError::OutOfGas,
                });
            }
        };

        let mut fields = Vec::with_capacity(num_elems);

        for bytes in input.chunks(32) {
            let le_bytes: Vec<u8> = bytes.iter().rev().copied().collect();
            if let Ok(res) = BLSScalar::from_bytes(&le_bytes) {
                fields.push(res);
            } else {
                return Err(PrecompileFailure::Error {
                    exit_status: ExitError::Other(
                        "Cannot convert bytes to field elements".into(),
                    ),
                });
            }
        }

        let mut res = AnemoiJive381::eval_variable_length_hash(&fields).to_bytes();
        res.reverse();

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
    EvalJive = "eval_jive(bytes32,bytes32,bytes32,uint256)",
    EvalVariableLengthHash = "eval_variable_length_hash(bytes32[])",
}

impl PrecompileId for Anemoi {
    fn contract_id() -> u64 {
        0x2002
    }
}

impl Precompile for Anemoi {
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
            Call::EvalJive => Self::eval_jive(input.get_slice(), target_gas),
            Call::EvalVariableLengthHash => {
                Self::eval_variable_length_hash(input.get_slice(), target_gas)
            }
        }
    }
}
