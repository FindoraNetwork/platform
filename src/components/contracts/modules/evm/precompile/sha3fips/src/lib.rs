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

use evm::executor::stack::PrecompileFailure;
use evm::ExitSucceed;
use module_evm::precompile::{LinearCostPrecompile, PrecompileId};
use tiny_keccak::Hasher;

pub struct Sha3FIPS256;

impl PrecompileId for Sha3FIPS256 {
    fn contract_id() -> u64 {
        0x7
    }
}

impl LinearCostPrecompile for Sha3FIPS256 {
    const BASE: u64 = 60;
    const WORD: u64 = 12;

    fn execute(
        input: &[u8],
        _: u64,
    ) -> core::result::Result<(ExitSucceed, Vec<u8>), PrecompileFailure> {
        let mut output = [0; 32];
        let mut sha3 = tiny_keccak::Sha3::v256();
        sha3.update(input);
        sha3.finalize(&mut output);
        Ok((ExitSucceed::Returned, output.to_vec()))
    }
}

pub struct Sha3FIPS512;

impl PrecompileId for Sha3FIPS512 {
    fn contract_id() -> u64 {
        0x8
    }
}

impl LinearCostPrecompile for Sha3FIPS512 {
    const BASE: u64 = 60;
    const WORD: u64 = 12;

    fn execute(
        input: &[u8],
        _: u64,
    ) -> core::result::Result<(ExitSucceed, Vec<u8>), PrecompileFailure> {
        let mut output = [0; 64];
        let mut sha3 = tiny_keccak::Sha3::v512();
        sha3.update(input);
        sha3.finalize(&mut output);
        Ok((ExitSucceed::Returned, output.to_vec()))
    }
}