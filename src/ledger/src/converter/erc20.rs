//! Multi Signer operation for transaction.

use crate::data_model::{
    NoReplayToken, Operation, Transaction, ASSET_TYPE_FRA, BLACK_HOLE_PUBKEY_STAKING,
};
use ethabi::Contract;
use fp_types::{crypto::MultiSigner, H160, U256};
use lazy_static::lazy_static;
use ruc::*;
use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use zei::xfr::{
    sig::{XfrKeyPair, XfrPublicKey, XfrSignature},
    structs::{AssetType as ZeiAssetType, XfrAssetType},
};

#[allow(missing_docs)]
pub struct ContractConstructor {
    pub abi: Contract,
    pub code: Vec<u8>,
}

impl ContractConstructor {
    #[inline(always)]
    #[allow(missing_docs)]
    pub fn load() -> Self {
        #[cfg(feature = "debug_env")]
        let reader = std::fs::File::open("/tmp/findora/abi/ERC20.abi").unwrap();

        #[cfg(not(feature = "debug_env"))]
        let reader = std::fs::File::open("abi/ERC20.abi").unwrap();

        let abi = ethabi::Contract::load(reader).unwrap();

        Self {
            abi,
            //code: hex::decode(include!("abi/ERC20.bin")).unwrap(),
            code: vec![],
        }
    }
}

lazy_static! {
    #[allow(missing_docs)]
    pub static ref ERC20_CONSTRUCTOR: ContractConstructor = ContractConstructor::load();
}

/// Use this operation to transfer.
///
/// This operation only support binded xfr_address is sender address.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct TransferERC20 {
    /// transaction body
    pub data: Data,
    /// transaction signer
    pub public: XfrPublicKey,
    /// transaction signature
    pub signature: XfrSignature,
}

#[allow(missing_docs)]
impl TransferERC20 {
    pub fn new(
        keypair: &XfrKeyPair,
        nonce: NoReplayToken,
        address: MultiSigner,
    ) -> Self {
        let data = Data::new(nonce, address);
        let public = keypair.get_pk();
        let signature = keypair.sign(&data.to_bytes());
        Self {
            data,
            public,
            signature,
        }
    }

    pub fn verify(&self) -> Result<()> {
        self.public
            .verify(&self.data.to_bytes(), &self.signature)
            .c(d!())
    }

    pub fn set_nonce(&mut self, nonce: NoReplayToken) {
        self.data.nonce = nonce;
    }

    pub fn get_nonce(&self) -> NoReplayToken {
        self.data.nonce
    }

    pub fn get_related_address(&self) -> XfrPublicKey {
        self.public
    }
}

/// The body of TranserToAccount.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Data {
    /// transaction nonce
    pub nonce: NoReplayToken,
    /// receiver address
    pub address: MultiSigner,
}

#[allow(missing_docs)]
impl Data {
    pub fn new(nonce: NoReplayToken, address: MultiSigner) -> Self {
        Data { nonce, address }
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        pnk!(bincode::serialize(self))
    }
}

#[allow(missing_docs)]
pub fn is_transfer_erc20_tx(tx: &Transaction) -> bool {
    if tx.body.operations.len() == 2 {
        return matches!(
            tx.body.operations[..],
            [Operation::TransferAsset(_), Operation::TransferERC20(_)]
        );
    }
    false
}

/// build a input to call `decimals` function
pub fn build_decimals_input() -> Result<Vec<u8>> {
    ERC20_CONSTRUCTOR
        .abi
        .function("decimals")
        .c(d!("No decimals functions"))?
        .encode_input(&[])
        .c(d!("Failed to encode function input"))
}

#[allow(missing_docs)]
pub fn check_erc20_tx(tx: &Transaction) -> Result<(H160, Vec<u8>, ZeiAssetType)> {
    let mut owner = None;
    let mut asset = None;
    let mut amount = 0u64;

    for op in &tx.body.operations {
        if let Operation::TransferERC20(ca) = op {
            if owner.is_some() {
                return Err(eg!("tx should have only one convert account"));
            }
            owner = H160::try_from(ca.data.address.clone()).ok();
        }
        if let Operation::TransferAsset(t) = op {
            for o in &t.body.outputs {
                // FIXME: If we can relax this restriction?
                // both amount and asset type are no-confidential
                if !o.record.is_public() {
                    return Err(eg!(
                        "Findora custom asset cross ledger transfer not support confidential"
                    ));
                }
                // burn findora custom asset
                if let XfrAssetType::NonConfidential(ty) = o.record.asset_type {
                    if o.record.public_key == *BLACK_HOLE_PUBKEY_STAKING
                        && ty != ASSET_TYPE_FRA
                    {
                        if let Some(am) = o.record.amount.get_amount() {
                            amount += am;
                            if asset.is_some() && asset != Some(ty) {
                                return Err(eg!("Only one custom asset is supported"));
                            } else if asset.is_none() {
                                asset = Some(ty);
                            }
                        }
                    }
                }
            }
        }
    }

    if owner.is_none() || asset.is_none() {
        return Err(eg!("this isn't a valid utxo-to-erc20 tx"));
    }

    // check mint input
    let mint = ERC20_CONSTRUCTOR
        .abi
        .function("mint")
        .c(d!("No mint function"))?
        .encode_input(&[
            ethabi::Token::Address(owner.unwrap()),
            ethabi::Token::Uint(U256::from(amount)),
        ])
        .c(d!("Failed to encode mint input"))?;

    Ok((owner.unwrap(), mint, asset.unwrap()))
}
