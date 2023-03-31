// Copyright 2019-2021 PureStake Inc.
// This file is part of Moonbeam.

// Moonbeam is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// Moonbeam is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Moonbeam. If not, see <http://www.gnu.org/licenses/>.

use super::{error, EvmResult};
use core::{any::type_name, ops::Range};
use ethereum_types::{H256, U256};
use num::{BigUint, ToPrimitive};
use std::{fmt::Debug, vec, vec::Vec};
use tracing::warn;

pub struct Bytes(Vec<u8>);
pub trait ToBytes {
    fn to_bytes(self) -> Bytes;
}
pub trait AsVec {
    fn to_vec(&self) -> Vec<u8>;
    fn as_slice(&self) -> &[u8];
}

impl ToBytes for Vec<u8> {
    fn to_bytes(self) -> Bytes {
        let bytes = Bytes(self);
        return bytes;
    }
}

impl ToBytes for &[u8] {
    fn to_bytes(self) -> Bytes {
        let bytes = Bytes(self.to_owned().clone());
        return bytes;
    }
}

impl AsVec for Bytes {
    fn to_vec(&self) -> Vec<u8> {
        let vec = self.0.clone();
        return vec;
    }

    fn as_slice(&self) -> &[u8] {
        let vec = &self.0;
        return vec;
    }
}

/// Wrapper around an EVM input slice, helping to parse it.
/// Provide functions to parse common types.
#[derive(Clone, Copy, Debug)]
pub struct EvmDataReader2<'a> {
    pub input: &'a [u8],
    pub cursor: usize,
}

impl<'a> EvmDataReader2<'a> {
    /// Create a new input parser.
    pub fn new(input: &'a [u8]) -> Self {
        Self { input, cursor: 0 }
    }

    /// Check the input has at least the correct amount of arguments before the end (32 bytes values).
    pub fn expect_arguments(&self, args: usize) -> EvmResult {
        if self.input.len() >= self.cursor + args * 32 {
            Ok(())
        } else {
            Err(error("input doesn't match expected length"))
        }
    }

    /// Read data from the input.
    pub fn read<T: EvmData2>(&mut self) -> EvmResult<T> {
        T::read(self)
    }

    /// Read raw bytes from the input.
    /// Doesn't handle any alignement checks, prefer using `read` instead of possible.
    /// Returns an error if trying to parse out of bounds.
    pub fn read_raw_bytes(&mut self, len: usize) -> EvmResult<&[u8]> {
        let range = self.move_cursor(len)?;

        let data = self
            .input
            .get(range)
            .ok_or_else(|| error("tried to parse raw bytes out of bounds"))?;

        Ok(data)
    }

    /// Parse (4 bytes) selector.
    /// Returns an error if trying to parse out of bounds.
    pub fn read_selector<T>(&mut self) -> EvmResult<T>
    where
        T: num_enum::TryFromPrimitive<Primitive = u32>,
    {
        let mut buffer = [0u8; 4];
        buffer.copy_from_slice(
            self.read_raw_bytes(4)
                .map_err(|_| error("tried to parse selector out of bounds"))?,
        );
        T::try_from_primitive(u32::from_be_bytes(buffer)).map_err(|_| {
            warn!(
                target: "precompile",
                "Failed to match function selector for {}",
                type_name::<T>()
            );
            error("unknown selector")
        })
    }

    /// Move the reading cursor with provided length, and return a range from the previous cursor
    /// location to the new one.
    /// Checks cursor overflows.
    fn move_cursor(&mut self, len: usize) -> EvmResult<Range<usize>> {
        let start = self.cursor;
        let end = self
            .cursor
            .checked_add(len)
            .ok_or_else(|| error("data reading cursor overflow"))?;

        self.cursor = end;

        Ok(start..end)
    }
}

/// Help build an EVM input/output data.
#[derive(Clone, Debug)]
pub struct EvmDataWriter2 {
    pub(crate) data: Vec<u8>,
    arrays: Vec<Array2>,
}

#[derive(Clone, Debug)]
struct Array2 {
    offset_position: usize,
    data: Vec<u8>,
    inner_arrays: Vec<Array2>,
}

impl EvmDataWriter2 {
    /// Creates a new empty output builder.
    pub fn new() -> Self {
        Self {
            data: vec![],
            arrays: vec![],
        }
    }

    /// Return the built data.
    pub fn build(mut self) -> Vec<u8> {
        Self::build_arrays(&mut self.data, self.arrays, 0);

        self.data
    }

    /// Build the array into data.
    /// `global_offset` represents the start of the frame we are modifying.
    /// While the main data will have a `global_offset` of 0, inner arrays will have a
    /// `global_offset` corresponding to the start its parent array size data.
    fn build_arrays(output: &mut Vec<u8>, arrays: Vec<Array2>, global_offset: usize) {
        for mut array in arrays {
            let offset_position = array.offset_position;
            let offset_position_end = offset_position + 32;
            let free_space_offset = output.len() + global_offset;

            // Override dummy offset to the offset it will be in the final output.
            U256::from(free_space_offset)
                .to_big_endian(&mut output[offset_position..offset_position_end]);

            // Build inner arrays if any.
            Self::build_arrays(&mut array.data, array.inner_arrays, free_space_offset);

            // Append this data at the end of the current output.
            output.append(&mut array.data);
        }
    }

    /// Write arbitrary bytes.
    /// Doesn't handle any alignement checks, prefer using `write` instead if possible.
    pub fn write_raw_bytes(mut self, value: &[u8]) -> Self {
        self.data.extend_from_slice(value);
        self
    }

    /// Write a selector.
    /// The provided type must impl `Into<u32>`.
    /// Doesn't handle any alignement checks, should be used only when adding the initial
    /// selector of a Solidity call data.
    pub fn write_selector<T: Into<u32>>(self, value: T) -> Self {
        self.write_raw_bytes(&value.into().to_be_bytes())
    }

    /// Write data of requested type.
    pub fn write<T: EvmData2>(mut self, value: T) -> Self {
        T::write(&mut self, value);
        self
    }
}

impl Default for EvmDataWriter2 {
    fn default() -> Self {
        Self::new()
    }
}

/// Data that can be converted from and to EVM data types.
pub trait EvmData2: Sized {
    fn read(reader: &mut EvmDataReader2) -> EvmResult<Self>;
    fn write(writer: &mut EvmDataWriter2, value: Self);
}

impl EvmData2 for H256 {
    fn read(reader: &mut EvmDataReader2) -> EvmResult<Self> {
        let range = reader.move_cursor(32)?;

        let data = reader
            .input
            .get(range)
            .ok_or_else(|| error("tried to parse H256 out of bounds"))?;

        Ok(H256::from_slice(data))
    }

    fn write(writer: &mut EvmDataWriter2, value: Self) {
        writer.data.extend_from_slice(value.as_bytes());
    }
}

impl EvmData2 for U256 {
    fn read(reader: &mut EvmDataReader2) -> EvmResult<Self> {
        let range = reader.move_cursor(32)?;

        let data = reader
            .input
            .get(range)
            .ok_or_else(|| error("tried to parse U256 out of bounds"))?;

        Ok(U256::from_big_endian(data))
    }

    fn write(writer: &mut EvmDataWriter2, value: Self) {
        let mut buffer = [0u8; 32];
        value.to_big_endian(&mut buffer);
        writer.data.extend_from_slice(&buffer);
    }
}

impl EvmData2 for bool {
    fn read(reader: &mut EvmDataReader2) -> EvmResult<Self> {
        let h256 = H256::read(reader)
            .map_err(|_| error("tried to parse bool out of bounds"))?;

        Ok(!h256.is_zero())
    }

    fn write(writer: &mut EvmDataWriter2, value: Self) {
        let mut buffer = [0u8; 32];
        if value {
            buffer[31] = 1;
        }

        writer.data.extend_from_slice(&buffer);
    }
}

// The implementation for u8 is specific, for performance reasons.
impl EvmData2 for Bytes {
    fn read(reader: &mut EvmDataReader2) -> EvmResult<Self> {
        let mut start = U256::read(reader)?.as_usize() + 32;

        if start + 4 > reader.input.len() {
            return Err(error("Bytes read length out of bounds1"));
        }
        let len_data = &reader.input[start..start + 4];

        let len = BigUint::from_bytes_be(len_data);
        let len = len
            .to_usize()
            .ok_or(error("Bytes read length out of bounds2"))?;
        start += 4;

        if start + len > reader.input.len() {
            return Err(error("Bytes read length out of bounds3"));
        }
        let data = &reader.input[start..start + len];
        Ok(Bytes(data.to_owned()))
    }

    fn write(writer: &mut EvmDataWriter2, value: Self) {
        let offset_position = writer.data.len();
        H256::write(writer, H256::repeat_byte(0xff));
        // 0xff = When debugging it makes spoting offset values easier.

        let mut inner_writer = EvmDataWriter2::new();

        // Write length.
        inner_writer = inner_writer.write(U256::from(value.0.len()));

        let mut i = 0;
        while i < value.0.len() {
            let mut end = i + 32;
            if end > value.0.len() {
                end = value.0.len();
            }
            let src_buffer = &value.0[i..end];
            let mut buffer = [0u8; 32];
            buffer[..src_buffer.len()].copy_from_slice(src_buffer);
            inner_writer.data.extend_from_slice(&buffer);

            i += 32;
        }

        // // Write elements of array.
        // for inner in value2 {
        //     inner_writer = inner_writer.write(inner);
        // }

        let array = Array2 {
            offset_position,
            data: inner_writer.data,
            inner_arrays: inner_writer.arrays,
        };

        writer.arrays.push(array);
    }
}

impl EvmData2 for Vec<Bytes> {
    fn read(reader: &mut EvmDataReader2) -> EvmResult<Self> {
        let mut start = U256::read(reader)?.as_usize() + 32;

        if start + 4 > reader.input.len() {
            return Err(error("Bytes read length out of bounds1"));
        }
        let data = &reader.input[start..start + 4];
        let array_len = BigUint::from_bytes_be(data);
        let array_len = array_len
            .to_usize()
            .ok_or(error("Vec<Bytes> read length out of bounds2"))?;
        start += 32;

        let mut starts: Vec<usize> = Vec::new();
        let mut i = 0;
        while i < array_len {
            if start + (i * 32) + 4 > reader.input.len() {
                return Err(error("Bytes read length out of bounds3"));
            }
            let data_start = &reader.input[start + (i * 32)..start + (i * 32) + 4];
            let start = BigUint::from_bytes_be(data_start);
            let start = start
                .to_usize()
                .ok_or(error("Vec<Bytes> read length out of bounds4"))?;
            starts.push(start);
            i += 1;
        }

        let mut lens: Vec<usize> = Vec::new();
        for vstart in &starts {
            let start = start + vstart;
            if start + 4 > reader.input.len() {
                return Err(error("Bytes read length out of bounds5"));
            }
            let data1 = &reader.input[start..start + 4];
            let len = BigUint::from_bytes_be(data1);
            let len = len
                .to_usize()
                .ok_or(error("Vec<Bytes> read length out of bounds6"))?;
            lens.push(len);
        }

        let mut datas: Vec<Bytes> = Vec::new();
        i = 0;
        while i < array_len {
            let start = start + starts[i] + 4;
            if start + lens[i] > reader.input.len() {
                return Err(error("Bytes read length out of bounds7"));
            }
            let data = &reader.input[start..start + lens[i]];
            datas.push(data.to_bytes());
            i += 1;
        }

        Ok(datas)
    }

    fn write(writer: &mut EvmDataWriter2, value: Self) {
        let offset_position = writer.data.len();
        H256::write(writer, H256::repeat_byte(0xff));
        // 0xff = When debugging it makes spoting offset values easier.

        let mut inner_writer = EvmDataWriter2::new();

        // Write length.
        inner_writer = inner_writer.write(U256::from(value.len()));

        // Write elements of array.
        for inner in value {
            inner_writer = inner_writer.write(inner);
        }

        let array = Array2 {
            offset_position,
            data: inner_writer.data,
            inner_arrays: inner_writer.arrays,
        };

        writer.arrays.push(array);
    }
}
