
use chrono::prelude::*;
use zei::utxo_transaction::{Tx, TxOutput, TxAddressParams};
use zei::keys::{ZeiSignature, ZeiPublicKey};
use zei::serialization;
use serde::{Serialize, Deserialize};
use serde::{Serializer, Deserializer};
use std::hash::{Hash, Hasher};
use std::collections::hash_map::DefaultHasher;

// Unique Identifier for AssetTokens
#[derive(Default, Serialize, Deserialize, Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub struct AssetTokenCode {
    pub val: [u8; 16],
}

// TODO: Define Memo
#[derive(Default, Serialize, Deserialize, Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub struct Proof {}
#[derive(Default, Serialize, Deserialize, Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub struct Memo {}
#[derive(Default, Serialize, Deserialize, Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub struct ConfidentialMemo {}
pub type Commitment = [u8; 32];

#[derive(Default, Eq, PartialEq, Clone, Copy, Debug, Serialize, Deserialize)]
pub struct Address {
    #[serde(with = "serialization::zei_obj_serde")]
    pub key: ZeiPublicKey
}

#[derive(Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct LedgerSignature {
    pub address: Address,

    #[serde(with = "serialization::zei_obj_serde")]
    pub signature: ZeiSignature,
}

impl LedgerSignature {
    pub fn verify<>(&self, message: &[u8]) -> bool {
        !self.address.key.verify::<blake2::Blake2b>(message, &self.signature).is_err()
    }}


#[derive(Default, Eq, PartialEq, Copy, Clone, Debug, Serialize, Deserialize)]
pub struct AssetTokenProperties {
    pub code: AssetTokenCode,
    pub issuer: Address,
    pub memo: Memo,
    pub confidential_memo: ConfidentialMemo,
    pub updatable: bool,
}

#[derive(Default, Eq, PartialEq, Copy, Clone, Debug, Serialize, Deserialize)]
pub struct AssetToken {
    pub properties: AssetTokenProperties,
    pub digest: [u8; 32],
    pub units: u64,
    pub confidential_units: Commitment,
}

//impl AssetToken {
//    pub fn create_empty() -> AssetToken {
//        AssetToken {
//            code: AssetTokenCode{val:[0;16]},
//            digest: [0;32],
//            issuer: Address{key:[0;32]},
//            memo: Memo{},
//            confidential_memo: ConfidentialMemo{},
//            updatable: false,
//            units: 0,
//            confidential_units: [0;32],
//        }
//    }
//}

#[derive(Hash, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct AssetPolicyKey {
    pub val: [u8; 16],
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct CustomAssetPolicy {}

#[derive(Hash, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct CredentialKey {
    pub val: [u8; 16],
}

#[derive(Hash, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Credential {
    pub key: CredentialKey,
}

#[derive(Hash, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct SmartContractKey {
    pub val: [u8; 16],
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct SmartContract {}

//TODO(Kevin): define types
#[derive(Clone, Serialize, Deserialize)]
pub struct Variable {}

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug, Serialize, Deserialize)]
pub struct TxSequenceNumber {
    pub val: u64,
}

#[derive(Hash, Eq, PartialEq, Clone, Copy, Debug, Serialize, Deserialize)]
pub struct UtxoAddress {
    pub transaction_id: TxSequenceNumber,
    pub operation_index: u16,
    pub output_index: u16,
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct Asset {
    pub code: AssetTokenCode,
    pub amount: u64,
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct PrivateAsset {
    pub hidden: [u8; 32],
    //commitment values
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub enum AssetType {
    Normal(Asset),
    Private(PrivateAsset),
}

#[derive(Eq, PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct Utxo {
    pub key: UtxoAddress, //includes ledger address
    pub output: TxOutput, //will include public key
    pub digest: [u8; 32],
}

#[derive(Debug, Serialize, Deserialize)]
pub struct AssetTransferBody {
    //pub nonce: u128,
    pub inputs: Vec<UtxoAddress>,
    pub transfer: Tx,
    pub operation_signatures: Vec<LedgerSignature>,
}


#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct AssetIssuanceBody {
    pub seq_num: u128,
    pub code: AssetTokenCode,
    pub outputs: Vec<TxOutput>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct AssetCreationBody {
    pub properties: AssetTokenProperties,
}


//TODO: compute digests of each of these...
// impl Hash for AssetIssuanceBody {
//     fn hash<H: Hasher>(&self, state: &mut H) {
//         self.seq_num.hash(state);
//         self.code.hash(state);
//     }
// }

// impl Hash for AssetTransferBody {
//     fn hash<H: Hasher>(&self, state: &mut H) {
//         self.inputs.hash(state);
//     }
// }

// impl Hash for AssetCreationBody {
//     fn hash<H: Hasher>(&self, state: &mut H) {
//         self.properties.code.hash(state);
//         self.properties.memo.hash(state);
//     }
// }

// TODO: UTXO Addresses must be included in Transfer Signature
#[derive(Debug, Serialize, Deserialize)]
pub struct AssetTransfer {
    //pub nonce: u128,
    pub body: AssetTransferBody,
    pub body_signatures: Vec<LedgerSignature>,
}

// TODO: Include mechanism for replay attacks
#[derive(Debug, Serialize, Deserialize)]
pub struct AssetIssuance {
    pub body: AssetIssuanceBody,
    pub body_signature: LedgerSignature,
}

// ... etc...
#[derive(Debug, Serialize, Deserialize)]
pub struct AssetCreation {
    pub body: AssetCreationBody,
    pub body_signature: LedgerSignature,
}

#[derive(Debug, Serialize, Deserialize)]
pub enum Operation {
    asset_transfer(AssetTransfer),
    asset_issuance(AssetIssuance),
    asset_creation(AssetCreation),
    // ... etc...
}

#[derive(Debug)]
pub struct TimeBounds {
    pub start: DateTime<Utc>,
    pub end: DateTime<Utc>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Transaction {
    pub operations: Vec<Operation>,
    pub variable_utxos: Vec<UtxoAddress>,
    pub proofs: Vec<Proof>,
    pub memos: Vec<Memo>,
    //pub time_bounds: TimeBounds,
    // ... etc...
}

impl Transaction {
    pub fn create_empty() -> Transaction {
        Transaction {
            operations: Vec::new(),
            variable_utxos: Vec::new(),
            proofs: Vec::new(),
            memos: Vec::new(),
        }
    }
}
