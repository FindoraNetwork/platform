use super::solidity::*;
use baseapp::BaseApp;
use ethereum::TransactionAction;
use ethereum_types::{H160, U256};
use fp_mocks::*;
use fp_traits::evm::FeeCalculator;
use std::path::{Path, PathBuf};

pub const DEFAULT_GAS_LIMIT: u64 = 5000000;

pub struct ERC20Constructor(pub ContractConstructor);

impl From<ERC20Constructor> for ContractConstructor {
    fn from(c: ERC20Constructor) -> Self {
        c.0
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct ERC20(pub DeployedContract);

impl ERC20Constructor {
    pub fn load() -> Self {
        Self(ContractConstructor::compile_from_source(
            Path::new("tests").join("contracts"),
            Self::solidity_artifacts_path(),
            "ERC20.sol",
            "ERC20",
        ))
    }

    pub fn deploy(&self, name: &str, symbol: &str, nonce: U256) -> UnsignedTransaction {
        let input = self
            .0
            .abi
            .constructor()
            .unwrap()
            .encode_input(
                self.0.code.clone(),
                &[
                    ethabi::Token::String(name.to_string()),
                    ethabi::Token::String(symbol.to_string()),
                ],
            )
            .unwrap();
        UnsignedTransaction {
            nonce,
            gas_price: <BaseApp as module_evm::Config>::FeeCalculator::min_gas_price(),
            gas_limit: U256::from(DEFAULT_GAS_LIMIT),
            action: TransactionAction::Create,
            value: Default::default(),
            input,
        }
    }

    fn solidity_artifacts_path() -> PathBuf {
        //  Path::new("target").join("solidity_build")
        Path::new("tests").join("contracts").join("abi")
    }
}

impl ERC20 {
    pub fn mint(
        &self,
        recipient: H160,
        amount: U256,
        nonce: U256,
    ) -> UnsignedTransaction {
        let input = self
            .0
            .abi
            .function("mint")
            .unwrap()
            .encode_input(&[
                ethabi::Token::Address(recipient),
                ethabi::Token::Uint(amount),
            ])
            .unwrap();
        UnsignedTransaction {
            nonce,
            gas_price: <BaseApp as module_evm::Config>::FeeCalculator::min_gas_price(),
            gas_limit: U256::from(DEFAULT_GAS_LIMIT),
            action: TransactionAction::Call(self.0.address),
            value: Default::default(),
            input,
        }
    }

    pub fn transfer(
        &self,
        recipient: H160,
        amount: U256,
        nonce: U256,
        value: U256,
    ) -> UnsignedTransaction {
        let input = self
            .0
            .abi
            .function("transfer")
            .unwrap()
            .encode_input(&[
                ethabi::Token::Address(recipient),
                ethabi::Token::Uint(amount),
            ])
            .unwrap();
        UnsignedTransaction {
            nonce,
            gas_price: <BaseApp as module_evm::Config>::FeeCalculator::min_gas_price(),
            gas_limit: U256::from(DEFAULT_GAS_LIMIT),
            action: TransactionAction::Call(self.0.address),
            value,
            input,
        }
    }

    pub fn balance_of(&self, address: H160, nonce: U256) -> UnsignedTransaction {
        let input = self
            .0
            .abi
            .function("balanceOf")
            .unwrap()
            .encode_input(&[ethabi::Token::Address(address)])
            .unwrap();
        UnsignedTransaction {
            nonce,
            gas_price: <BaseApp as module_evm::Config>::FeeCalculator::min_gas_price(),
            gas_limit: U256::from(DEFAULT_GAS_LIMIT),
            action: TransactionAction::Call(self.0.address),
            value: Default::default(),
            input,
        }
    }
}
