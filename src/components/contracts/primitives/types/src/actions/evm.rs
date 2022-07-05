use primitive_types::{H160, H256, U256};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Action {
    Call(Call),
    Create(Create),
    Create2(Create2),
    CallEip1559(CallEip1559),
    CreateEip1559(CreateEip1559),
    Create2Eip1559(Create2Eip1559),
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Call {
    pub source: H160,
    pub target: H160,
    pub input: Vec<u8>,
    pub value: U256,
    pub gas_limit: u64,
    pub gas_price: Option<U256>,
    pub nonce: Option<U256>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CallEip1559 {
    pub source: H160,
    pub target: H160,
    pub input: Vec<u8>,
    pub value: U256,
    pub gas_limit: u64,
    pub max_fee_per_gas: Option<U256>,
    pub max_priority_fee_per_gas: Option<U256>,
    pub nonce: Option<U256>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Create {
    pub source: H160,
    pub init: Vec<u8>,
    pub value: U256,
    pub gas_limit: u64,
    pub gas_price: Option<U256>,
    pub nonce: Option<U256>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CreateEip1559 {
    pub source: H160,
    pub init: Vec<u8>,
    pub value: U256,
    pub gas_limit: u64,
    pub max_fee_per_gas: Option<U256>,
    pub max_priority_fee_per_gas: Option<U256>,
    pub nonce: Option<U256>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Create2 {
    pub source: H160,
    pub init: Vec<u8>,
    pub salt: H256,
    pub value: U256,
    pub gas_limit: u64,
    pub gas_price: Option<U256>,
    pub nonce: Option<U256>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Create2Eip1559 {
    pub source: H160,
    pub init: Vec<u8>,
    pub salt: H256,
    pub value: U256,
    pub gas_limit: u64,
    pub max_fee_per_gas: Option<U256>,
    pub max_priority_fee_per_gas: Option<U256>,
    pub nonce: Option<U256>,
}
