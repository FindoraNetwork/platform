#![cfg_attr(test, allow(unused_imports))]

pub use abci::Application;
pub use baseapp::{
    extensions::{CheckFee, CheckNonce, SignedExtra},
    BaseApp,
};
pub use fp_types::{actions::Action, assemble::UncheckedTransaction};

use ethereum::{TransactionAction, TransactionSignature, TransactionV0 as Transaction};
use fp_traits::account::AccountAsset;
use fp_traits::evm::{AddressMapping, EthereumAddressMapping};
use fp_types::crypto::{Address, MultiSignature};
use lazy_static::lazy_static;
use primitive_types::{H160, H256, U256};
use rand_chacha::{rand_core::SeedableRng, ChaChaRng};
use rlp::*;
use sha3::{Digest, Keccak256};
use std::env::temp_dir;
use std::path::PathBuf;
use std::sync::Mutex;
use std::time::SystemTime;
use zei::xfr::sig::XfrKeyPair;

lazy_static! {
    pub static ref BASE_APP: Mutex<BaseApp> = Mutex::new(
        BaseApp::new(create_temp_db_path().as_path(), false, (0, None), false).unwrap()
    );
    pub static ref ALICE_ECDSA: KeyPair = generate_address(1);
    pub static ref BOB_ECDSA: KeyPair = generate_address(2);
    pub static ref ALICE_XFR: XfrKeyPair =
        XfrKeyPair::generate(&mut ChaChaRng::from_entropy());
    pub static ref BOB_XFR: XfrKeyPair =
        XfrKeyPair::generate(&mut ChaChaRng::from_entropy());
}

pub fn test_mint_balance(who: &Address, balance: U256, height: u64) {
    assert!(module_account::App::<BaseApp>::mint(
        &BASE_APP.lock().unwrap().deliver_state,
        who,
        balance
    )
    .is_ok());
    BASE_APP
        .lock()
        .unwrap()
        .deliver_state
        .state
        .clone()
        .write()
        .commit(height)
        .unwrap();

    let ctx = BASE_APP
        .lock()
        .unwrap()
        .create_query_context(Some(height), false)
        .unwrap();
    assert_eq!(module_account::App::<BaseApp>::balance(&ctx, who), balance);
}

pub fn build_signed_transaction(
    function: Action,
    who: &XfrKeyPair,
    nonce: U256,
) -> UncheckedTransaction<SignedExtra> {
    let extra = (CheckNonce::new(nonce), CheckFee::new(None));

    let signer: Address = who.get_pk().into();
    let msg = serde_json::to_vec(&(function.clone(), extra.clone())).unwrap();
    let sig = who.get_sk_ref().sign(msg.as_slice(), who.get_pk_ref());
    let signature = MultiSignature::from(sig);

    UncheckedTransaction::new_signed(function, signer, signature, extra)
}

pub fn create_temp_db_path() -> PathBuf {
    let time = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    let mut path = temp_dir();
    path.push(format!("temp-findora-db–{}", time));
    path
}

pub struct KeyPair {
    pub address: H160,
    pub private_key: H256,
    pub account_id: Address,
}

pub fn generate_address(seed: u8) -> KeyPair {
    let private_key = H256::from_slice(&[(seed + 1) as u8; 32]);
    let secret_key = libsecp256k1::SecretKey::parse_slice(&private_key[..]).unwrap();
    let public_key =
        &libsecp256k1::PublicKey::from_secret_key(&secret_key).serialize()[1..65];
    let address = H160::from(H256::from_slice(&Keccak256::digest(public_key)[..]));

    KeyPair {
        address,
        private_key,
        account_id: EthereumAddressMapping::convert_to_account_id(address),
    }
}

pub struct UnsignedTransaction {
    pub nonce: U256,
    pub gas_price: U256,
    pub gas_limit: U256,
    pub action: TransactionAction,
    pub value: U256,
    pub input: Vec<u8>,
}

impl UnsignedTransaction {
    fn signing_rlp_append(&self, s: &mut RlpStream, chain_id: u64) {
        s.begin_list(9);
        s.append(&self.nonce);
        s.append(&self.gas_price);
        s.append(&self.gas_limit);
        s.append(&self.action);
        s.append(&self.value);
        s.append(&self.input);
        s.append(&chain_id);
        s.append(&0u8);
        s.append(&0u8);
    }

    fn signing_hash(&self, chain_id: u64) -> H256 {
        let mut stream = RlpStream::new();
        self.signing_rlp_append(&mut stream, chain_id);
        H256::from_slice(Keccak256::digest(&stream.out()).as_slice())
    }

    pub fn sign(&self, key: &H256, chain_id: u64) -> Transaction {
        let hash = self.signing_hash(chain_id);
        let msg = libsecp256k1::Message::parse(hash.as_fixed_bytes());
        let s = libsecp256k1::sign(
            &msg,
            &libsecp256k1::SecretKey::parse_slice(&key[..]).unwrap(),
        );
        let sig = s.0.serialize();

        let sig = TransactionSignature::new(
            s.1.serialize() as u64 % 2 + chain_id * 2 + 35,
            H256::from_slice(&sig[0..32]),
            H256::from_slice(&sig[32..64]),
        )
        .unwrap();

        Transaction {
            nonce: self.nonce,
            gas_price: self.gas_price,
            gas_limit: self.gas_limit,
            action: self.action,
            value: self.value,
            input: self.input.clone(),
            signature: sig,
        }
    }
}
