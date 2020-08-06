use crate::LedgerStateCommitment;
use ledger::data_model::{b64enc, Asset, AssetType, AuthenticatedUtxo, TxoSID};
use serde::de::DeserializeOwned;
use snafu::{ensure, Backtrace, ResultExt, Snafu};
use std::process::exit;
use std::time::Duration;
use structopt::clap::{Error, ErrorKind};
use utils::HashOf;
use zeroize::Zeroizing;

/// Computes a http client with a specific timeout
fn get_client() -> Result<reqwest::blocking::Client, reqwest::Error> {
  const TIMEOUT: u64 = 20;
  let client = reqwest::blocking::Client::builder().timeout(Duration::from_secs(TIMEOUT))
                                                   .build()?;
  Ok(client)
}

pub fn do_request_asset(query: &str) -> Result<Asset, Error> {
  // TODO Phlippe how to avoid code duplication for obtaining the http client?
  let client_res = get_client();
  let client: reqwest::blocking::Client;
  match client_res {
    Err(_e) => {
      return Err(Error::with_description("Unable to establish connection.", ErrorKind::Io))
    }
    _ => client = client_res.unwrap(),
  }

  let resp = match client.get(query).send() {
    Err(e) => {
      eprintln!("Request `{}` failed: {}", query, e);
      exit(-1);
    }
    Ok(resp) => match resp.json::<AssetType>() {
      Err(e) => {
        eprintln!("Problem parsing response {}, {}", query, e);
        return Err(Error::with_description("Problem parsing json", ErrorKind::Format));
        // TODO find a more informative error
      }
      Ok(v) => v.properties,
    },
  };

  Ok(resp)
}

pub fn do_request<T: DeserializeOwned>(query: &str) -> Result<T, Error> {
  // TODO see above
  let client_res = get_client();
  let client: reqwest::blocking::Client;
  match client_res {
    Err(_e) => {
      return Err(Error::with_description("Unable to establish connection.", ErrorKind::Io))
    }
    _ => client = client_res.unwrap(),
  }

  let resp: T = match client.get(query).send() {
    Err(e) => {
      eprintln!("Request `{}` failed: {}", query, e);
      exit(-1);
    }
    Ok(resp) => match resp.json::<T>() {
      Err(e) => {
        eprintln!("Problem parsing response {}, {}", query, e);
        return Err(Error::with_description("Problem parsing json", ErrorKind::Io));
        // TODO find a more informative error
      }
      Ok(v) => v,
    },
  };

  Ok(resp)
}

pub fn do_request_authenticated_utxo(query: &str,
                                     sid: u64,
                                     ledger_state: &LedgerStateCommitment)
                                     -> Result<AuthenticatedUtxo, Error> {
  // TODO: see above
  let client_res = get_client();
  let client: reqwest::blocking::Client;
  match client_res {
    Err(_e) => {
      return Err(Error::with_description("Unable to establish connection.", ErrorKind::Io))
    }
    _ => client = client_res.unwrap(),
  }

  let resp = match client.get(query).send() {
    Err(e) => {
      eprintln!("Request `{}` failed: {}", query, e);
      exit(-1); // TODO Philippe
    }
    Ok(resp) => match resp.json::<AuthenticatedUtxo>() {
      Err(e) => {
        eprintln!("Problem parsing response {}, {}", query, e);
        return Err(Error::with_description("Problem parsing json", ErrorKind::Io));
        // TODO Philippe find a more informative error
      }

      Ok(v) => {
        let resp_comm = HashOf::new(&Some(v.state_commitment_data.clone()));
        let curr_comm = (ledger_state.0).0.clone();
        if resp_comm != curr_comm {
          eprintln!("Server responded with authentication relative to `{}`!",
                    b64enc(&resp_comm.0.hash));
          eprintln!("The most recent ledger state I have is `{}`.",
                    b64enc(&curr_comm.0.hash));
          eprintln!("Please run query-ledger-state then rerun this command.");
          exit(-1); // TODO Philippe return some error
        }

        // TODO: this needs better direct authentication
        if v.authenticated_spent_status.utxo_sid != TxoSID(sid) {
          eprintln!("!!!!! ERROR !!!!!!");
          eprintln!("The server responded with a different UTXO sid.");
          eprintln!("This could indicate a faulty server, or a man-in-the-middle!");
          eprintln!("\nFor safety, refusing to update.");
          exit(-1); // TODO  Philippe return some error
        }
        if !v.is_valid((ledger_state.0).0.clone()) {
          eprintln!("!!!!! ERROR !!!!!!");
          eprintln!("The server responded with an invalid authentication proof.");
          eprintln!("This could indicate a faulty server, or a man-in-the-middle!");
          eprintln!("\nFor safety, refusing to update.");
          exit(-1); // TODO Philippe return some error
        }
        v
      }
    },
  };
  Ok(resp)
}

#[derive(Snafu, Debug)]
pub enum PasswordReadError {
  #[snafu(display("Entered passwords did not match."))]
  DidNotMatch { backtrace: Backtrace },
  #[snafu(display("Failed getting user input."))]
  UserInput {
    source: std::io::Error,
    backtrace: Backtrace,
  },
}

/// Reads a user's password without confirming
///
/// Optionally takes a string describing what the password is for
pub fn prompt_password(description: Option<&str>) -> Result<Zeroizing<String>, PasswordReadError> {
  let prompt = if let Some(s) = description {
    format!("Enter password for {}: ", s)
  } else {
    "Enter Password: ".to_string()
  };

  rpassword::prompt_password_stdout(&prompt).context(UserInput)
                                            .map(Zeroizing::new)
}

/// Reads a password from the user twice, and confirms that they match
///
/// Optionally takes a string describing what the password is for
pub fn prompt_password_confirming(description: Option<&str>)
                                  -> Result<Zeroizing<String>, PasswordReadError> {
  let first_prompt = if let Some(s) = description {
    format!("Enter password for {}: ", s)
  } else {
    "Enter Password: ".to_string()
  };

  let first = rpassword::prompt_password_stdout(&first_prompt).context(UserInput)
                                                              .map(Zeroizing::new)?;

  let second = rpassword::prompt_password_stdout("Enter password again:").context(UserInput)
                                                                         .map(Zeroizing::new)?;
  // Return an error if the entered passwords did not match
  ensure!(first == second, DidNotMatch);
  Ok(first)
}
