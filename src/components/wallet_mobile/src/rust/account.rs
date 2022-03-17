use ledger::data_model::ASSET_TYPE_FRA;
use zei::xfr::sig::XfrKeyPair;

use super::transaction::TransactionBuilder;

use fp_types::{
    actions::{
        xhub::{
            Action as AccountAction, NonConfidentialOutput, NonConfidentialTransfer,
        },
        Action,
    },
    assemble::{CheckFee, CheckNonce, SignedExtra, UncheckedTransaction},
    crypto::{Address, MultiSignature, MultiSigner},
    U256,
};

use fp_utils::{ecdsa::SecpPair, tx::EvmRawTxWrapper};

use core::str::FromStr;

use ruc::Result as RucResult;
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
    ) -> RucResult<Self> {
        let target_address = match address {
            Some(s) => MultiSigner::from_str(&s)?,
            None => MultiSigner::Xfr(keypair.get_pk()),
        };

        self.get_builder_mut()
            .add_operation_convert_account(keypair, target_address, amount)?
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
    pub fn new_transfer_from_account(
        amount: u64,
        address: Option<String>,
        fra_kp: &XfrKeyPair,
        eth_phrase: Option<String>,
        nonce: U256,
    ) -> RucResult<EVMTransactionBuilder> {
        let target = match address {
            Some(s) => {
                if let Ok(address) = globutils::wallet::public_key_from_base64(&s) {
                    address
                } else {
                    globutils::wallet::public_key_from_bech32(&s)?
                }
            }
            None => fra_kp.get_pk(),
        };

        let output = NonConfidentialOutput {
            target,
            amount,
            asset: ASSET_TYPE_FRA,
        };

        let (signer, keypair) = if let Some(key_path) = eth_phrase {
            let kp = SecpPair::from_phrase(&key_path, None)?.0;
            let signer = Address::from(kp.address());
            (signer, Keypair::Ecdsa(kp))
        } else {
            let signer = Address::from(fra_kp.get_pk());
            (signer, Keypair::Ed25519(fra_kp.clone()))
        };

        let account_call =
            AccountAction::NonConfidentialTransfer(NonConfidentialTransfer {
                input_value: amount,
                outputs: vec![output],
            });
        let action = Action::XHub(account_call);
        let extra = (CheckNonce::new(nonce), CheckFee::new(None));
        let msg = serde_json::to_vec(&(&action, &extra)).unwrap();

        let signature = keypair.sign(msg.as_slice());

        let tx_unchecked =
            UncheckedTransaction::new_signed(action, signer, signature, extra);

        let tx = EVMTransactionKind::Unchecked(tx_unchecked);

        Ok(EVMTransactionBuilder { tx })
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
