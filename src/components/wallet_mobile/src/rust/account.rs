use core::str::FromStr;
use ledger::data_model::ASSET_TYPE_FRA;
use ruc::{d, Result, RucResult};
use zei::xfr::sig::{XfrKeyPair, XfrPublicKey};

use super::transaction::TransactionBuilder;

use fp_types::{
    actions::{
        xhub::{Action as XhubAction, NonConfidentialOutput, NonConfidentialTransfer},
        Action,
    },
    assemble::{CheckFee, CheckNonce, SignedExtra, UncheckedTransaction},
    crypto::{Address, MultiSignature, MultiSigner},
    U256,
};

use fp_utils::{ecdsa::SecpPair, tx::EvmRawTxWrapper};

#[allow(missing_docs)]
pub enum Keypair {
    Ed25519(XfrKeyPair),
    Ecdsa(SecpPair),
}

#[allow(missing_docs)]
impl Keypair {
    pub fn sign(&self, data: &[u8]) -> MultiSignature {
        match self {
            Keypair::Ecdsa(kp) => MultiSignature::from(kp.sign(data)),
            Keypair::Ed25519(kp) => {
                MultiSignature::from(kp.get_sk_ref().sign(data, kp.get_pk_ref()))
            }
        }
    }
}

impl TransactionBuilder {
    /// transfer utxo assets to account(ed25519 or ecdsa address) balance.
    pub fn add_transfer_to_account_operation(
        mut self,
        amount: u64,
        address: Option<String>,
        keypair: &XfrKeyPair,
    ) -> Result<Self> {
        let target_address = match address {
            Some(s) => MultiSigner::from_str(&s)?,
            None => MultiSigner::Xfr(keypair.get_pk()),
        };

        self.get_builder_mut()
            .add_operation_convert_account(keypair, target_address, amount, None, None)?
            .sign(keypair);

        Ok(self)
    }
}

pub struct EVMTransactionBuilder {
    tx: EVMTransactionKind,
}

pub enum EVMTransactionKind {
    Unchecked(UncheckedTransaction<SignedExtra>),
}

impl EVMTransactionBuilder {
    /// transfer to uxto assets from account(ed25519 or ecdsa address) balance.

    pub fn new_transfer_to_utxo_from_account(
        recipient: XfrPublicKey,
        amount: u64,
        sk: String,
        nonce: U256,
    ) -> Result<String> {
        let seed = hex::decode(sk).c(d!())?;
        let mut s = [0u8; 32];
        s.copy_from_slice(&seed);
        let kp = SecpPair::from_seed(&s);

        let output = NonConfidentialOutput {
            target: recipient,
            amount,
            asset: ASSET_TYPE_FRA,
            decimal: 6,
            max_supply: 0,
        };
        let action = Action::XHub(XhubAction::NonConfidentialTransfer(
            NonConfidentialTransfer {
                input_value: amount,
                outputs: vec![output],
            },
        ));

        let extra = (CheckNonce::new(nonce), CheckFee::new(None));
        let msg = serde_json::to_vec(&(action.clone(), extra.clone())).c(d!())?;
        let signature = MultiSignature::from(kp.sign(&msg));
        let signer = Address::from(kp.address());

        let tx = UncheckedTransaction::new_signed(action, signer, signature, extra);
        let res = serde_json::to_string(&tx).c(d!())?;

        let tx_with_tag = EvmRawTxWrapper::wrap(res.as_bytes());
        String::from_utf8(tx_with_tag).c(d!())
    }

    pub fn serialized_transaction_base64(&self) -> String {
        let txn = match &self.tx {
            EVMTransactionKind::Unchecked(tx_unchecked) => {
                serde_json::to_vec(tx_unchecked).unwrap()
            }
        };

        let txn_with_tag = EvmRawTxWrapper::wrap(&txn);
        base64::encode(&txn_with_tag)
    }

    pub fn into_ptr(self) -> *mut Self {
        Box::into_raw(Box::new(self))
    }

    /// # Safety
    /// Construct `EVMTransactionBuilder` from raw pointer.
    pub unsafe fn from_ptr(raw: *mut EVMTransactionBuilder) -> Box<Self> {
        Box::from_raw(raw)
    }
}

/// Serialize ethereum address used to abci query nonce.
pub fn get_serialized_address(address: &str) -> Result<String> {
    let ms = MultiSigner::from_str(address)?;
    let account: Address = ms.into();
    let sa = serde_json::to_string(&account).unwrap();
    Ok(sa)
}
