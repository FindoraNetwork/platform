use crate::{print_conf, CliDataStore, CliError, LedgerStateCommitment};
use ledger::data_model::*;
use ledger_api_service::LedgerAccessRoutes;
use promptly::prompt_default;
use std::process::exit;
use utils::NetworkRoute;
use utils::{HashOf, SignatureOf};
use zei::xfr::sig::XfrPublicKey;

pub fn query_ledger_state<S: CliDataStore>(store: &mut S,
                                           forget_old_key: bool)
                                           -> Result<(), CliError> {
  store.update_config(|conf| {
         let mut new_key = forget_old_key;
         if !new_key && conf.ledger_sig_key.is_none() {
           println!("No signature key found for `{}`.", conf.ledger_server);
           new_key = new_key || prompt_default(" Retrieve a new one?", false).unwrap();
           if !new_key {
             eprintln!("Cannot check ledger state validity without a signature key.");
             exit(-1);
           }
         }

         if new_key {
           let query = format!("{}{}",
                               conf.ledger_server,
                               LedgerAccessRoutes::PublicKey.route());
           let resp: XfrPublicKey;
           match reqwest::blocking::get(&query) {
             Err(e) => {
               eprintln!("Request `{}` failed: {}", query, e);
               exit(-1);
             }
             Ok(v) => match v.text()
                             .map(|x| serde_json::from_str::<XfrPublicKey>(&x).map_err(|e| (x, e)))
             {
               Err(e) => {
                 eprintln!("Failed to decode response: {}", e);
                 exit(-1);
               }
               Ok(Err((x, e))) => {
                 eprintln!("Failed to parse response `{}`: {}", x, e);
                 exit(-1);
               }
               Ok(Ok(v)) => {
                 resp = v;
               }
             },
           }

           println!("Saving ledger signing key `{}`",
                    serde_json::to_string(&resp).unwrap());
           conf.ledger_sig_key = Some(resp);
         }

         assert!(conf.ledger_sig_key.is_some());

         let query = format!("{}{}",
                             conf.ledger_server,
                             LedgerAccessRoutes::GlobalState.route());
         let resp: (HashOf<Option<StateCommitmentData>>,
                    u64,
                    SignatureOf<(HashOf<Option<StateCommitmentData>>, u64)>);
         match reqwest::blocking::get(&query) {
           Err(e) => {
             eprintln!("Request `{}` failed: {}", query, e);
             exit(-1);
           }
           Ok(v) => match v.text()
                           .map(|x| serde_json::from_str::<_>(&x).map_err(|e| (x, e)))
           {
             Err(e) => {
               eprintln!("Failed to decode response: {}", e);
               exit(-1);
             }
             Ok(Err((x, e))) => {
               eprintln!("Failed to parse response `{}`: {}", x, e);
               exit(-1);
             }
             Ok(Ok(v)) => {
               resp = v;
             }
           },
         }

         if let Err(e) = resp.2
                             .verify(&conf.ledger_sig_key.unwrap(), &(resp.0.clone(), resp.1))
         {
           eprintln!("Ledger responded with invalid signature: {}", e);
           exit(-1);
         }

         conf.ledger_state = Some(LedgerStateCommitment(resp));

         assert!(conf.ledger_state.is_some());

         println!("New state retrieved.");

         print_conf(&conf);
       })?;
  Ok(())
}
