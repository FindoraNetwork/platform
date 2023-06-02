use ethabi::{decode, encode, ParamType, Token};
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

        // params: decode(bytes32,bytes32,bytes32,uint256);
        let mut tokens = decode(
            &[
                ParamType::FixedBytes(32),
                ParamType::FixedBytes(32),
                ParamType::FixedBytes(32),
                ParamType::Uint(256),
            ],
            input,
        )
        .map_err(|_| PrecompileFailure::Error {
            exit_status: ExitError::Other("invalid bytes".into()),
        })?;
        let index = tokens.pop().unwrap().into_uint().unwrap(); // safe
        let f2 = tokens.pop().unwrap().into_fixed_bytes().unwrap(); // safe
        let f1 = tokens.pop().unwrap().into_fixed_bytes().unwrap(); // safe
        let f0 = tokens.pop().unwrap().into_fixed_bytes().unwrap(); // safe

        let field0 =
            BLSScalar::from_bytes(&f0).map_err(|_| PrecompileFailure::Error {
                exit_status: ExitError::Other("invalid field".into()),
            })?;
        let field1 =
            BLSScalar::from_bytes(&f1).map_err(|_| PrecompileFailure::Error {
                exit_status: ExitError::Other("invalid field".into()),
            })?;
        let field2 =
            BLSScalar::from_bytes(&f2).map_err(|_| PrecompileFailure::Error {
                exit_status: ExitError::Other("invalid field".into()),
            })?;

        let res = AnemoiJive381::eval_jive(
            &[field0, field1],
            &[field2, ANEMOI_JIVE_381_SALTS[index.as_usize()]],
        )
        .to_bytes();

        // return: encode(bytes32);
        let hash = Token::FixedBytes(res);
        let bytes = encode(&[hash]);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            cost: gas_cost,
            output: bytes,
            logs: Default::default(),
        })
    }

    pub fn eval_variable_length_hash(
        input: &[u8],
        target_gas: Option<u64>,
    ) -> PrecompileResult {
        // params: decode(bytes32[]);
        let mut tokens = decode(
            &[ParamType::Array(Box::new(ParamType::FixedBytes(32)))],
            input,
        )
        .map_err(|_| PrecompileFailure::Error {
            exit_status: ExitError::Other("invalid bytes".into()),
        })?;

        let mut fields = vec![];
        for i in tokens.pop().unwrap().into_array().unwrap() {
            let field_bytes = i.into_fixed_bytes().unwrap(); // safe;
            let field = BLSScalar::from_bytes(&field_bytes).map_err(|_| {
                PrecompileFailure::Error {
                    exit_status: ExitError::Other("invalid field".into()),
                }
            })?;
            fields.push(field);
        }

        let gas_cost = fields.len() as u64 * Self::GAS_PER_PERM;
        if let Some(gas_left) = target_gas {
            if gas_left < gas_cost {
                return Err(PrecompileFailure::Error {
                    exit_status: ExitError::OutOfGas,
                });
            }
        };

        let res = AnemoiJive381::eval_variable_length_hash(&fields).to_bytes();

        // return: encode(bytes32);
        let hash = Token::FixedBytes(res);
        let bytes = encode(&[hash]);

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            cost: gas_cost,
            output: bytes,
            logs: Default::default(),
        })
    }
}

#[evm_precompile_utils::generate_function_selector]
#[derive(Debug, PartialEq, Eq, num_enum::TryFromPrimitive, num_enum::IntoPrimitive)]
pub enum Call {
    EvalJive = "evalJive(bytes32,bytes32,bytes32,uint256)",
    EvalVariableLengthHash = "evalVariableLengthHash(bytes32[])",
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
