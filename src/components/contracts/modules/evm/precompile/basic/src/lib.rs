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

use core::cmp::min;
use evm::{executor::stack::PrecompileFailure, ExitError, ExitSucceed};
use module_evm::precompile::{LinearCostPrecompile, PrecompileId};

/// The identity precompile.
pub struct Identity;

impl PrecompileId for Identity {
    fn contract_id() -> u64 {
        0x4
    }
}

impl LinearCostPrecompile for Identity {
    const BASE: u64 = 15;
    const WORD: u64 = 3;

    fn execute(
        input: &[u8],
        _: u64,
    ) -> core::result::Result<(ExitSucceed, Vec<u8>), PrecompileFailure> {
        Ok((ExitSucceed::Returned, input.to_vec()))
    }
}

/// The ecrecover precompile.
pub struct ECRecover;

impl PrecompileId for ECRecover {
    fn contract_id() -> u64 {
        0x1
    }
}

impl LinearCostPrecompile for ECRecover {
    const BASE: u64 = 3000;
    const WORD: u64 = 0;

    fn execute(
        i: &[u8],
        _: u64,
    ) -> core::result::Result<(ExitSucceed, Vec<u8>), PrecompileFailure> {
        let mut input = [0u8; 128];
        input[..min(i.len(), 128)].copy_from_slice(&i[..min(i.len(), 128)]);

        let mut msg = [0u8; 32];
        let mut sig = [0u8; 65];

        msg[0..32].copy_from_slice(&input[0..32]);
        sig[0..32].copy_from_slice(&input[64..96]);
        sig[32..64].copy_from_slice(&input[96..128]);
        sig[64] = input[63];

        let result = match fp_types::crypto::secp256k1_ecdsa_recover(&sig, &msg) {
            Ok(pubkey) => {
                let mut address = fp_utils::hashing::keccak_256(&pubkey);
                address[0..12].copy_from_slice(&[0u8; 12]);
                address.to_vec()
            }
            Err(_) => [0u8; 0].to_vec(),
        };

        Ok((ExitSucceed::Returned, result))
    }
}

/// The ripemd precompile.
pub struct Ripemd160;

impl PrecompileId for Ripemd160 {
    fn contract_id() -> u64 {
        0x3
    }
}

impl LinearCostPrecompile for Ripemd160 {
    const BASE: u64 = 600;
    const WORD: u64 = 120;

    fn execute(
        input: &[u8],
        _cost: u64,
    ) -> core::result::Result<(ExitSucceed, Vec<u8>), PrecompileFailure> {
        use ripemd160::Digest;

        let mut ret = [0u8; 32];
        ret[12..32].copy_from_slice(&ripemd160::Ripemd160::digest(input));
        Ok((ExitSucceed::Returned, ret.to_vec()))
    }
}

/// The sha256 precompile.
pub struct Sha256;

impl PrecompileId for Sha256 {
    fn contract_id() -> u64 {
        0x2
    }
}

impl LinearCostPrecompile for Sha256 {
    const BASE: u64 = 60;
    const WORD: u64 = 12;

    fn execute(
        input: &[u8],
        _cost: u64,
    ) -> core::result::Result<(ExitSucceed, Vec<u8>), PrecompileFailure> {
        let ret = fp_utils::hashing::sha2_256(input);
        Ok((ExitSucceed::Returned, ret.to_vec()))
    }
}

/// The ECRecoverPublicKey precompile.
/// Similar to ECRecover, but returns the pubkey (not the corresponding Ethereum address)
pub struct ECRecoverPublicKey;

impl PrecompileId for ECRecoverPublicKey {
    fn contract_id() -> u64 {
        0x6
    }
}

impl LinearCostPrecompile for ECRecoverPublicKey {
    const BASE: u64 = 3000;
    const WORD: u64 = 0;

    fn execute(
        i: &[u8],
        _: u64,
    ) -> core::result::Result<(ExitSucceed, Vec<u8>), PrecompileFailure> {
        let mut input = [0u8; 128];
        input[..min(i.len(), 128)].copy_from_slice(&i[..min(i.len(), 128)]);

        let mut msg = [0u8; 32];
        let mut sig = [0u8; 65];

        msg[0..32].copy_from_slice(&input[0..32]);
        sig[0..32].copy_from_slice(&input[64..96]);
        sig[32..64].copy_from_slice(&input[96..128]);
        sig[64] = input[63];

        let pubkey =
            fp_types::crypto::secp256k1_ecdsa_recover(&sig, &msg).map_err(|_| {
                PrecompileFailure::Error {
                    exit_status: ExitError::Other("Public key recover failed".into()),
                }
            })?;

        Ok((ExitSucceed::Returned, pubkey.to_vec()))
    }
}
