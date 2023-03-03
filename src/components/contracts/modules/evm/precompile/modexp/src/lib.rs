// SPDX-License-Identifier: Apache-2.0
// This file is part of Frontier.
//
// Copyright (c) 2020 Parity Technologies (UK) Ltd.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

use core::cmp::max;
use evm::{
    executor::stack::{PrecompileFailure, PrecompileHandle, PrecompileOutput},
    // Context,
    ExitError,
    ExitSucceed,
};
use module_evm::precompile::{FinState, Precompile, PrecompileId, PrecompileResult};
use num::{BigUint, FromPrimitive, One, ToPrimitive, Zero};

pub struct Modexp;

impl PrecompileId for Modexp {
    fn contract_id() -> u64 {
        0x5
    }
}

const MIN_GAS_COST: u64 = 200;

// Calculate gas cost according to EIP 2565:
// https://eips.ethereum.org/EIPS/eip-2565
fn calculate_gas_cost(
    base_length: u64,
    mod_length: u64,
    exponent: &BigUint,
    exponent_bytes: &[u8],
) -> u64 {
    fn calculate_multiplication_complexity(base_length: u64, mod_length: u64) -> u64 {
        let max_length = max(base_length, mod_length);
        let mut words = max_length / 8;
        if max_length % 8 > 0 {
            words += 1;
        }

        // Note: can't overflow because we take words to be some u64 value / 8, which is
        // necessarily less than sqrt(u64::MAX).
        // Additionally, both base_length and mod_length are bounded to 1024, so this has
        // an upper bound of roughly (1024 / 8) squared
        words * words
    }

    fn calculate_iteration_count(exponent: &BigUint, exponent_bytes: &[u8]) -> u64 {
        let mut iteration_count: u64 = 0;
        let exp_length = exponent_bytes.len() as u64;

        if exp_length <= 32 && exponent.is_zero() {
            iteration_count = 0;
        } else if exp_length <= 32 {
            iteration_count = exponent.bits() - 1;
        } else if exp_length > 32 {
            // from the EIP spec:
            // (8 * (exp_length - 32)) + ((exponent & (2**256 - 1)).bit_length() - 1)
            //
            // Notes:
            // * exp_length is bounded to 1024 and is > 32
            // * exponent can be zero, so we subtract 1 after adding the other terms (whose sum
            //   must be > 0)
            // * the addition can't overflow because the terms are both capped at roughly
            //   8 * max size of exp_length (1024)
            // * the EIP spec is written in python, in which (exponent & (2**256 - 1)) takes the
            //   FIRST 32 bytes. However this `BigUint` `&` operator takes the LAST 32 bytes.
            //   We thus instead take the bytes manually.
            let exponent_head = BigUint::from_bytes_be(&exponent_bytes[..32]);

            iteration_count = (8 * (exp_length - 32)) + exponent_head.bits() - 1;
        }

        max(iteration_count, 1)
    }

    let multiplication_complexity =
        calculate_multiplication_complexity(base_length, mod_length);
    let iteration_count = calculate_iteration_count(exponent, exponent_bytes);
    max(
        MIN_GAS_COST,
        multiplication_complexity * iteration_count / 3,
    )
}

/// Copy bytes from input to target.
fn read_input(source: &[u8], target: &mut [u8], source_offset: &mut usize) {
    // We move the offset by the len of the target, regardless of what we
    // actually copy.
    let offset = *source_offset;
    *source_offset += target.len();

    // Out of bounds, nothing to copy.
    if source.len() <= offset {
        return;
    }

    // Find len to copy up to target len, but not out of bounds.
    let len = core::cmp::min(target.len(), source.len() - offset);
    target[..len].copy_from_slice(&source[offset..][..len]);
}

// ModExp expects the following as inputs:
// 1) 32 bytes expressing the length of base
// 2) 32 bytes expressing the length of exponent
// 3) 32 bytes expressing the length of modulus
// 4) base, size as described above
// 5) exponent, size as described above
// 6) modulus, size as described above
//
//
// NOTE: input sizes are bound to 1024 bytes, with the expectation
//       that gas limits would be applied before actual computation.
//
//       maximum stack size will also prevent abuse.
//
//       see: https://eips.ethereum.org/EIPS/eip-198

impl Precompile for Modexp {
    fn execute(
        handle: &mut impl PrecompileHandle,
        _state: &FinState,
    ) -> PrecompileResult {
        let input = handle.input();
        let mut input_offset = 0;

        // Yellowpaper: whenever the input is too short, the missing bytes are
        // considered to be zero.
        let mut base_len_buf = [0u8; 32];
        read_input(input, &mut base_len_buf, &mut input_offset);
        let mut exp_len_buf = [0u8; 32];
        read_input(input, &mut exp_len_buf, &mut input_offset);
        let mut mod_len_buf = [0u8; 32];
        read_input(input, &mut mod_len_buf, &mut input_offset);

        // reasonable assumption: this must fit within the Ethereum EVM's max stack size
        let max_size_big = BigUint::from_u32(1024).expect("can't create BigUint");

        let base_len_big = BigUint::from_bytes_be(&base_len_buf);
        if base_len_big > max_size_big {
            return Err(PrecompileFailure::Error {
                exit_status: ExitError::Other("unreasonably large base length".into()),
            });
        }

        let exp_len_big = BigUint::from_bytes_be(&exp_len_buf);
        if exp_len_big > max_size_big {
            return Err(PrecompileFailure::Error {
                exit_status: ExitError::Other(
                    "unreasonably large exponent length".into(),
                ),
            });
        }

        let mod_len_big = BigUint::from_bytes_be(&mod_len_buf);
        if mod_len_big > max_size_big {
            return Err(PrecompileFailure::Error {
                exit_status: ExitError::Other(
                    "unreasonably large modulus length".into(),
                ),
            });
        }

        // bounds check handled above
        let base_len = base_len_big.to_usize().expect("base_len out of bounds");
        let exp_len = exp_len_big.to_usize().expect("exp_len out of bounds");
        let mod_len = mod_len_big.to_usize().expect("mod_len out of bounds");

        // if mod_len is 0 output must be empty
        if mod_len == 0 {
            return Ok(PrecompileOutput {
                exit_status: ExitSucceed::Returned,
                output: vec![],
            });
        }

        // Gas formula allows arbitrary large exp_len when base and modulus are empty, so we need to handle empty base first.
        let r = if base_len == 0 && mod_len == 0 {
            handle.record_cost(MIN_GAS_COST)?;
            BigUint::zero()
        } else {
            // read the numbers themselves.
            let mut base_buf = vec![0u8; base_len];
            read_input(input, &mut base_buf, &mut input_offset);
            let base = BigUint::from_bytes_be(&base_buf);

            let mut exp_buf = vec![0u8; exp_len];
            read_input(input, &mut exp_buf, &mut input_offset);
            let exponent = BigUint::from_bytes_be(&exp_buf);

            let mut mod_buf = vec![0u8; mod_len];
            read_input(input, &mut mod_buf, &mut input_offset);
            let modulus = BigUint::from_bytes_be(&mod_buf);

            // do our gas accounting
            let gas_cost =
                calculate_gas_cost(base_len as u64, mod_len as u64, &exponent, &exp_buf);

            handle.record_cost(gas_cost)?;

            if modulus.is_zero() || modulus.is_one() {
                BigUint::zero()
            } else {
                base.modpow(&exponent, &modulus)
            }
        };

        // write output to given memory, left padded and same length as the modulus.
        let bytes = r.to_bytes_be();

        // always true except in the case of zero-length modulus, which leads to
        // output of length and value 1.
        if bytes.len() == mod_len {
            Ok(PrecompileOutput {
                exit_status: ExitSucceed::Returned,
                output: bytes.to_vec(),
            })
        } else if bytes.len() < mod_len {
            let mut ret = Vec::with_capacity(mod_len);
            ret.extend(core::iter::repeat(0).take(mod_len - bytes.len()));
            ret.extend_from_slice(&bytes[..]);
            Ok(PrecompileOutput {
                exit_status: ExitSucceed::Returned,
                output: ret.to_vec(),
            })
        } else {
            Err(PrecompileFailure::Error {
                exit_status: ExitError::Other("failed".into()),
            })
        }
    }
}
