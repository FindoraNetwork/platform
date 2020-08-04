use crate::LedgerStateCommitment;
use ledger::data_model::{b64enc, Asset, AssetType, AuthenticatedUtxo, TxoSID};
use serde::de::DeserializeOwned;
use std::process::exit;
use structopt::clap::Error;
use structopt::clap::ErrorKind;
use utils::HashOf;

pub fn do_request_asset(query: &str) -> Asset {
  let resp: Asset;

  match reqwest::blocking::get(query) {
    Err(e) => {
      eprintln!("Request `{}` failed: {}", query, e);
      exit(-1);
    }
    Ok(v) => match v.text()
                    .map(|x| serde_json::from_str::<AssetType>(&x).map_err(|e| (x, e)))
    {
      Err(e) => {
        eprintln!("Failed to decode response: {}", e);
        exit(-1);
      }
      Ok(Err((x, e))) => {
        eprintln!("Failed to parse response `{}`: {}", x, e);
        exit(-1);
      }
      Ok(Ok(v)) => resp = v.properties,
    },
  };

  resp
}

pub fn do_request<T: DeserializeOwned>(query: &str) -> Result<T, Error> {
  let resp: T;

  match reqwest::blocking::get(query) {
    Err(e) => {
      eprintln!("Request `{}` failed: {}", query, e);
      exit(-1);
    }
    Ok(v) => match v.text()
                    .map(|x| serde_json::from_str::<T>(&x).map_err(|e| (x, e)))
    {
      Err(e) => {
        eprintln!("Failed to decode response: {}", e);
        return Err(Error::with_description(format!("Failed to decode response: {}", e).as_str(),
                                           ErrorKind::Io));
      }
      Ok(Err((x, e))) => {
        eprintln!("Failed to parse response `{}`: {}", x, e);
        return Err(Error::with_description(format!("Failed to parse response `{}`: {}", x, e).as_str(),
                                       ErrorKind::Io));
      }
      Ok(Ok(v)) => {
        resp = v;
      }
    },
  }

  Ok(resp)
}

pub fn do_request_authenticated_utxo(query: &str,
                                     sid: u64,
                                     ledger_state: &LedgerStateCommitment)
                                     -> AuthenticatedUtxo {
  let resp: AuthenticatedUtxo;

  match reqwest::blocking::get(query) {
    Err(e) => {
      eprintln!("Request `{}` failed: {}", query, e);
      exit(-1);
    }
    Ok(v) => match v.text()
                    .map(|x| serde_json::from_str::<AuthenticatedUtxo>(&x).map_err(|e| (x, e)))
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
        let resp_comm = HashOf::new(&Some(v.state_commitment_data.clone()));
        let curr_comm = (ledger_state.0).0.clone();
        if resp_comm != curr_comm {
          eprintln!("Server responded with authentication relative to `{}`!",
                    b64enc(&resp_comm.0.hash));
          eprintln!("The most recent ledger state I have is `{}`.",
                    b64enc(&curr_comm.0.hash));
          eprintln!("Please run query-ledger-state then rerun this command.");
          exit(-1);
        }

        // TODO: this needs better direct authentication
        if v.authenticated_spent_status.utxo_sid != TxoSID(sid) {
          eprintln!("!!!!! ERROR !!!!!!");
          eprintln!("The server responded with a different UTXO sid.");
          eprintln!("This could indicate a faulty server, or a man-in-the-middle!");
          eprintln!("\nFor safety, refusing to update.");
          exit(-1);
        }

        if !v.is_valid((ledger_state.0).0.clone()) {
          eprintln!("!!!!! ERROR !!!!!!");
          eprintln!("The server responded with an invalid authentication proof.");
          eprintln!("This could indicate a faulty server, or a man-in-the-middle!");
          eprintln!("\nFor safety, refusing to update.");
          exit(-1);
        }

        resp = v;
      }
    },
  }

  resp
}
