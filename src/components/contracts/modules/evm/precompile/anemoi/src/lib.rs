use evm::executor::stack::{PrecompileFailure, PrecompileOutput};
use evm::{Context, ExitError, ExitSucceed};
use module_evm::precompile::{FinState, Precompile, PrecompileId, PrecompileResult};
use platform_lib_noah::noah_algebra::{
    bls12_381::BLSScalar, bn254::BN254Scalar, prelude::Scalar,
};
#[allow(deprecated)]
use platform_lib_noah::noah_crypto::anemoi_jive::{
    bls12_381_deprecated::AnemoiJive381Deprecated, AnemoiJive, AnemoiJive254,
};

/// The Anemoi precompile for BLS12-381
pub struct Anemoi381;

impl Anemoi381 {
    pub const GAS_PER_PERM: u64 = 125000;

    #[allow(deprecated)]
    pub fn execute_with_input_and_gas(
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

        let mut field_elems = Vec::with_capacity(num_elems);

        for i in 0..num_elems {
            let res = BLSScalar::from_bytes(
                &input[i * 32..(i + 1) * 32]
                    .iter()
                    .rev()
                    .copied()
                    .collect::<Vec<u8>>(),
            );
            if let Ok(res) = res {
                field_elems.push(res);
            } else {
                return Err(PrecompileFailure::Error {
                    exit_status: ExitError::Other(
                        "Cannot convert bytes to field elements".into(),
                    ),
                });
            }
        }

        let mut res =
            AnemoiJive381Deprecated::eval_variable_length_hash(&field_elems).to_bytes();
        res.reverse();

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            cost: gas_cost,
            output: res.to_vec(),
            logs: Default::default(),
        })
    }
}

impl PrecompileId for Anemoi381 {
    fn contract_id() -> u64 {
        0x2002
    }
}

impl Precompile for Anemoi381 {
    fn execute(
        input: &[u8],
        target_gas: Option<u64>,
        _context: &Context,
        _state: &FinState,
    ) -> PrecompileResult {
        Self::execute_with_input_and_gas(input, target_gas)
    }
}

/// The Anemoi precompile for BN254
pub struct Anemoi254;

impl Anemoi254 {
    pub const GAS_PER_PERM: u64 = 125000;

    pub fn execute_with_input_and_gas(
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

        let mut field_elems = Vec::with_capacity(num_elems);

        for i in 0..num_elems {
            let res = BN254Scalar::from_bytes(
                &input[i * 32..(i + 1) * 32]
                    .iter()
                    .rev()
                    .copied()
                    .collect::<Vec<u8>>(),
            );
            if let Ok(res) = res {
                field_elems.push(res);
            } else {
                return Err(PrecompileFailure::Error {
                    exit_status: ExitError::Other(
                        "Cannot convert bytes to field elements".into(),
                    ),
                });
            }
        }

        let mut res = AnemoiJive254::eval_variable_length_hash(&field_elems).to_bytes();
        res.reverse();

        Ok(PrecompileOutput {
            exit_status: ExitSucceed::Returned,
            cost: gas_cost,
            output: res.to_vec(),
            logs: Default::default(),
        })
    }
}

impl PrecompileId for Anemoi254 {
    fn contract_id() -> u64 {
        0x2003
    }
}

impl Precompile for Anemoi254 {
    fn execute(
        input: &[u8],
        target_gas: Option<u64>,
        _context: &Context,
        _state: &FinState,
    ) -> PrecompileResult {
        Self::execute_with_input_and_gas(input, target_gas)
    }
}
