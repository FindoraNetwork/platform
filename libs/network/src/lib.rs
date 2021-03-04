#![allow(clippy::field_reassign_with_default)]

use ledger::data_model::{
    AssetType, AssetTypeCode, AuthenticatedKVLookup, AuthenticatedUtxo, BlockSID,
    FinalizedTransaction, KVBlind, StateCommitmentData, Transaction, TxoSID,
};
use ledger::store::LedgerState;
use ledger_api_service::{
    ActixLedgerClient, MockLedgerClient, RestfulArchiveAccess, RestfulLedgerAccess,
};
use query_api::{
    ActixQueryServerClient, MockQueryServerClient, RestfulQueryServerAccess,
};
use ruc::*;
use serde::Serialize;
use sparse_merkle_tree::Key;
use std::sync::{Arc, RwLock};
use submission_api::{ActixLUClient, MockLUClient, RestfulLedgerUpdate};
use submission_server::{TxnHandle, TxnStatus};
use utils::{HashOf, SignatureOf, LEDGER_PORT, QUERY_PORT, SUBMIT_PORT};
use zei::xfr::sig::XfrPublicKey;
use zei::xfr::structs::OwnerMemo;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

pub type MockLedgerStandalone =
    LedgerStandalone<MockLUClient, MockLedgerClient, MockQueryServerClient>;

pub struct LedgerStandalone<
    LU: RestfulLedgerUpdate,
    LA: RestfulLedgerAccess + RestfulArchiveAccess,
    Q: RestfulQueryServerAccess,
> {
    submission_server_client: LU,
    ledger_client: LA,
    query_server_client: Q,
}

pub struct HttpStandaloneConfig {
    ledger_port: usize,
    submit_port: usize,
    query_port: usize,
    host: String,
    protocol: String,
}

impl HttpStandaloneConfig {
    pub fn testnet() -> Self {
        HttpStandaloneConfig {
            ledger_port: LEDGER_PORT,
            submit_port: SUBMIT_PORT,
            query_port: QUERY_PORT,
            host: String::from("testnet.findora.org"),
            protocol: String::from("https"),
        }
    }

    pub fn local() -> Self {
        HttpStandaloneConfig {
            ledger_port: LEDGER_PORT,
            submit_port: SUBMIT_PORT,
            query_port: QUERY_PORT,
            host: String::from("localhost"),
            protocol: String::from("http"),
        }
    }
}

impl LedgerStandalone<MockLUClient, MockLedgerClient, MockQueryServerClient> {
    /// Creates a ledger standalone client that talks to fake servers, no ports required
    pub fn new_mock(block_capacity: usize) -> Self {
        let ledger_state = LedgerState::test_ledger();
        let state_lock = Arc::new(RwLock::new(ledger_state));
        let ledger_client = MockLedgerClient::new(&state_lock);
        let submission_server_client = MockLUClient::new(&state_lock, block_capacity);
        let query_server_client = MockQueryServerClient();

        LedgerStandalone {
            submission_server_client,
            query_server_client,
            ledger_client,
        }
    }
}

impl LedgerStandalone<ActixLUClient, ActixLedgerClient, ActixQueryServerClient> {
    /// Creates a ledger standalone client that communicates with live Findora servers
    pub fn new_http(config: &HttpStandaloneConfig) -> Self {
        let ledger_client =
            ActixLedgerClient::new(config.ledger_port, &config.host, &config.protocol);
        let submission_server_client =
            ActixLUClient::new(config.submit_port, &config.host, &config.protocol);
        let query_server_client = ActixQueryServerClient::new(
            config.query_port,
            &config.host,
            &config.protocol,
        );

        LedgerStandalone {
            submission_server_client,
            query_server_client,
            ledger_client,
        }
    }
}

impl<
    LU: RestfulLedgerUpdate,
    LA: RestfulLedgerAccess + RestfulArchiveAccess,
    Q: RestfulQueryServerAccess,
> RestfulArchiveAccess for LedgerStandalone<LU, LA, Q>
{
    fn get_blocks_since(
        &self,
        addr: BlockSID,
    ) -> Result<Vec<(usize, Vec<FinalizedTransaction>)>> {
        self.ledger_client.get_blocks_since(addr).c(d!())
    }

    fn get_source(&self) -> String {
        self.ledger_client.get_source()
    }
}

impl<
    LU: RestfulLedgerUpdate,
    LA: RestfulLedgerAccess + RestfulArchiveAccess,
    Q: RestfulQueryServerAccess,
> RestfulQueryServerAccess for LedgerStandalone<LU, LA, Q>
{
    fn store_custom_data(
        &mut self,
        data: &dyn AsRef<[u8]>,
        key: &Key,
        blind: Option<KVBlind>,
    ) -> Result<()> {
        self.query_server_client
            .store_custom_data(data, key, blind)
            .c(d!())
    }

    fn fetch_custom_data(&self, key: &Key) -> Result<Vec<u8>> {
        self.query_server_client.fetch_custom_data(key).c(d!())
    }

    fn get_owner_memo(&self, txo_sid: u64) -> Result<Option<OwnerMemo>> {
        self.query_server_client.get_owner_memo(txo_sid).c(d!())
    }
}

impl<
    LU: RestfulLedgerUpdate,
    LA: RestfulLedgerAccess + RestfulArchiveAccess,
    Q: RestfulQueryServerAccess,
> RestfulLedgerAccess for LedgerStandalone<LU, LA, Q>
{
    fn get_utxo(&self, addr: TxoSID) -> Result<AuthenticatedUtxo> {
        self.ledger_client.get_utxo(addr).c(d!())
    }

    fn get_issuance_num(&self, code: &AssetTypeCode) -> Result<u64> {
        self.ledger_client.get_issuance_num(code).c(d!())
    }

    fn get_asset_type(&self, code: &AssetTypeCode) -> Result<AssetType> {
        self.ledger_client.get_asset_type(code).c(d!())
    }

    #[allow(clippy::type_complexity)]
    fn get_state_commitment(
        &self,
    ) -> Result<(
        HashOf<Option<StateCommitmentData>>,
        u64,
        SignatureOf<(HashOf<Option<StateCommitmentData>>, u64)>,
    )> {
        self.ledger_client.get_state_commitment().c(d!())
    }

    #[allow(clippy::type_complexity)]
    fn get_block_commit_count(&self) -> Result<u64> {
        match self.ledger_client.get_state_commitment() {
            Ok((_, seq_id, _)) => Ok(seq_id),
            Err(e) => Err(eg!(e)),
        }
    }

    fn get_kv_entry(&self, addr: Key) -> Result<AuthenticatedKVLookup> {
        self.ledger_client.get_kv_entry(addr).c(d!())
    }

    fn public_key(&self) -> Result<XfrPublicKey> {
        self.ledger_client.public_key().c(d!())
    }

    fn sign_message<T: Serialize + serde::de::DeserializeOwned>(
        &self,
        msg: &T,
    ) -> Result<SignatureOf<T>> {
        self.ledger_client.sign_message(msg).c(d!())
    }
}

impl<
    LU: RestfulLedgerUpdate,
    LA: RestfulLedgerAccess + RestfulArchiveAccess,
    Q: RestfulQueryServerAccess,
> RestfulLedgerUpdate for LedgerStandalone<LU, LA, Q>
{
    fn submit_transaction(&mut self, txn: &Transaction) -> Result<TxnHandle> {
        self.submission_server_client
            .submit_transaction(txn)
            .c(d!())
    }
    fn force_end_block(&mut self) -> Result<()> {
        self.submission_server_client.force_end_block().c(d!())
    }
    fn txn_status(&self, handle: &TxnHandle) -> Result<TxnStatus> {
        self.submission_server_client.txn_status(handle).c(d!())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ledger::data_model::*;
    use rand_chacha::ChaChaRng;
    use rand_core::SeedableRng;
    use zei::xfr::sig::XfrKeyPair;

    #[test]
    fn test_mock_client() {
        let mut mock_rest_client = LedgerStandalone::new_mock(2);
        //let seq_id = mock_rest_client.get_block_commit_count().unwrap()
        let tx = Transaction::from_seq_id(0);
        let handle = mock_rest_client.submit_transaction(&tx).unwrap();
        mock_rest_client.force_end_block().unwrap();
        let status = mock_rest_client.txn_status(&handle).unwrap();
        if let TxnStatus::Committed(_comm) = status {
        } else {
            panic!();
        }
    }

    #[test]
    fn test_mock_client_failure() {
        let code = AssetTypeCode::gen_random();
        let new_memo = Memo("new_memo".to_string());
        let mut prng = ChaChaRng::from_entropy();
        let creator = XfrKeyPair::generate(&mut prng);
        let memo_update = UpdateMemo::new(
            UpdateMemoBody {
                new_memo,
                asset_type: code,
                no_replay_token: NoReplayToken::default(),
            },
            &creator,
        );
        let mut mock_rest_client = LedgerStandalone::new_mock(2);
        let tx = Transaction::from_operation(Operation::UpdateMemo(memo_update), 0);
        let handle = mock_rest_client.submit_transaction(&tx).unwrap();
        let status = mock_rest_client.txn_status(&handle).unwrap();
        if let TxnStatus::Rejected(_) = status {
        } else {
            panic!();
        }
    }

    #[quickcheck]
    #[ignore]
    #[test]
    fn test_define_asset(code: Vec<u8>) {
        let code = AssetTypeCode::new_from_vec(code);
        let mut ledger = LedgerStandalone::new_mock(1);
        let mut prng = ChaChaRng::from_entropy();
        let creator = XfrKeyPair::generate(&mut prng);
        println!("{:?}", code);

        let mut properties: Asset = Default::default();
        properties.code = code;
        properties.issuer.key = *creator.get_pk_ref();

        let body = DefineAssetBody {
            asset: Box::new(properties.clone()),
        };

        let op = DefineAsset::new(body, &IssuerKeyPair { keypair: &creator }).unwrap();
        let seq_id = ledger.get_block_commit_count().unwrap();
        let txn = Transaction {
            body: TransactionBody {
                no_replay_token: NoReplayToken::new(&mut prng, seq_id),
                operations: vec![Operation::DefineAsset(op)],
                credentials: vec![],
                memos: vec![],
                policy_options: None,
            },
            signatures: vec![],
        };

        let handle = ledger.submit_transaction(&txn).unwrap();
        if let TxnStatus::Rejected(_) = ledger.txn_status(&handle).unwrap() {
            panic!("A valid transaction failed!");
        }
        assert_eq!(ledger.get_asset_type(&code).unwrap().properties, properties);
    }
}
