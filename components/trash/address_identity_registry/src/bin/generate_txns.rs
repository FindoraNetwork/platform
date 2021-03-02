use clap::{App, Arg, ArgMatches};
use credentials::{
    credential_commit, credential_issuer_key_gen, credential_sign,
    credential_user_key_gen, CredCommitment, CredCommitmentKey, CredIssuerPublicKey,
    CredIssuerSecretKey, Credential,
};
use ledger::data_model::StateCommitmentData;
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use std::collections::HashMap;
use std::error::Error;
use txn_builder::{BuildsTransactions, TransactionBuilder};
use utils::{protocol_host, GlobalState, LEDGER_PORT};
use zei::xfr::sig::XfrKeyPair;

/// Represents a file that can be searched

struct AIR {
    prng: ChaChaRng,
    issuer_pk: CredIssuerPublicKey,
    issuer_sk: CredIssuerSecretKey,
    user_commitment: HashMap<String, (CredCommitment, CredCommitmentKey)>,
}

impl AIR {
    fn new() -> Self {
        let mut prng = ChaChaRng::from_entropy();
        let attrs = vec![
            (String::from("dob"), 8),
            (String::from("pob"), 3),
            (String::from("sex"), 1),
        ];
        let (issuer_pk, issuer_sk) = credential_issuer_key_gen(&mut prng, &attrs);
        AIR {
            prng,
            issuer_pk,
            issuer_sk,
            user_commitment: HashMap::new(),
        }
    }

    /// Return a JSON txn corresponding to this request
    pub fn make_assign_txn(&mut self, seq_id: u64) -> Option<String> {
        let (user_pk, user_sk) =
            credential_user_key_gen(&mut self.prng, &self.issuer_pk);
        let attr_vals: Vec<(String, &[u8])> = vec![
            (String::from("dob"), b"08221964"),
            (String::from("pob"), b"666"),
            (String::from("sex"), b"M"),
        ];
        let sig = credential_sign(&mut self.prng, &self.issuer_sk, &user_pk, &attr_vals)
            .unwrap();
        let credential = Credential {
            signature: sig,
            attributes: attr_vals
                .iter()
                .map(|(k, v)| (k.clone(), v.to_vec()))
                .collect(),
            issuer_pub_key: self.issuer_pk.clone(),
        };
        // let xfr_key_pair = XfrKeyPair::zei_from_bytes(&hex::decode(KEY_PAIR_STR).unwrap());
        let xfr_key_pair = XfrKeyPair::generate(&mut self.prng);
        let (commitment, proof, key) = credential_commit(
            &mut self.prng,
            &user_sk,
            &credential,
            xfr_key_pair.get_pk_ref().as_bytes(),
        )
        .unwrap();
        let mut txn_builder = TransactionBuilder::from_seq_id(seq_id);
        let user_pk_s = serde_json::to_string(&user_pk).unwrap();
        self.user_commitment
            .insert(user_pk_s, (commitment.clone(), key));
        if let Err(e) = txn_builder.add_operation_air_assign(
            &xfr_key_pair,
            user_pk,
            commitment,
            self.issuer_pk.clone(),
            proof,
        ) {
            println!("add_operation_air_assign failed with {:?}", e);
            None
        } else {
            let txn = txn_builder.transaction();
            let result = serde_json::to_string(&txn).unwrap();
            Some(result)
        }
    }
}

fn parse_args() -> ArgMatches<'static> {
    App::new("generate AIR assign transactions")
        .version("0.1.0")
        .author("Engineering <engineering@findora.org>")
        .about("REPL with argument parsing")
        .arg(
            Arg::with_name("num_txns")
                .short("n")
                .long("num_txns")
                .takes_value(true)
                .help("number of txns generated"),
        )
        .get_matches()
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = parse_args();
    let num_str = args.value_of("num_txns");
    let mut air = AIR::new();
    let (protocol, host) = protocol_host();
    let client = reqwest::blocking::Client::new();
    let resp_gs = client
        .get(&format!(
            "{}://{}:{}/global_state",
            protocol, host, LEDGER_PORT
        ))
        .send()?;
    let (_, seq_id, _): GlobalState<StateCommitmentData> =
        serde_json::from_str(&resp_gs.text()?[..]).unwrap();

    match num_str {
        None => println!("Missing '--num_txns <number>'"),
        Some(s) => match s.parse::<usize>() {
            Ok(n) => {
                for _ in 0..n {
                    if let Some(txn) = air.make_assign_txn(seq_id) {
                        println!("{}", txn);
                    }
                }
            }
            Err(_) => println!("That's not a number! {}", s),
        },
    }
    Ok(())
}
