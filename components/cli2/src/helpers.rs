use crate::{CliError, LedgerStateCommitment};
use ledger::data_model::{b64enc, Asset, AssetType, AuthenticatedUtxo, TxoSID};
use ruc::*;
use serde::de::DeserializeOwned;
use std::env;
use std::fmt;
use std::path::PathBuf;
use std::process::exit;
use std::time::Duration;
use structopt::clap::{Error, ErrorKind};
use utils::HashOf;
use zeroize::Zeroizing;

/// Computes a http client with a specific timeout
fn get_client() -> Result<reqwest::blocking::Client> {
    const TIMEOUT: u64 = 20;
    let client = reqwest::blocking::Client::builder()
        .timeout(Duration::from_secs(TIMEOUT))
        .build()
        .c(d!())?;
    Ok(client)
}

pub fn do_request_asset(query: &str) -> Result<Asset> {
    let client = get_client().c(d!(CliError::Reqwest))?;

    let resp = match client.get(query).send() {
        Err(e) => {
            eprintln!("Request '{}' failed: {}", query, e);
            return Err(eg!(CliError::Reqwest));
        }
        Ok(resp) => match resp.json::<AssetType>() {
            Err(e) => {
                eprintln!("Problem parsing response {}, {}", query, e);
                return Err(eg!(CliError::Reqwest));
            }
            Ok(v) => v.properties,
        },
    };

    Ok(resp)
}

pub fn do_request<T: DeserializeOwned>(query: &str) -> Result<T> {
    let client = get_client()
        .map_err(|_| {
            Error::with_description(
                "The http client failed being initialized.",
                ErrorKind::Io,
            )
        })
        .c(d!())?;

    let resp: T = match client.get(query).send() {
        Err(e) => {
            eprintln!("Request '{}' failed: {}", query, e);
            exit(-1);
        }
        Ok(resp) => match resp.json::<T>() {
            Err(e) => {
                if let Ok(Ok(msg)) = client.get(query).send().map(|r| r.text()) {
                    println!("\x1b[31;1m{}\x1b[0m", msg);
                } else {
                    eprintln!("Problem parsing response {}, {}", query, e);
                }
                return Err(eg!(Error::with_description(
                    "Problem parsing json",
                    ErrorKind::Io,
                )));
                // TODO find a more informative error
            }
            Ok(v) => v,
        },
    };

    Ok(resp)
}

pub fn do_request_authenticated_utxo(
    query: &str,
    sid: u64,
    ledger_state: &LedgerStateCommitment,
) -> Result<AuthenticatedUtxo> {
    let client = get_client().c(d!(CliError::Reqwest))?;

    let resp = match client.get(query).send() {
        Err(e) => {
            eprintln!("Request '{}' failed: {}", query, e);
            return Err(eg!(CliError::Reqwest));
        }
        Ok(resp) => {
            match resp.json::<AuthenticatedUtxo>() {
                Err(e) => {
                    eprintln!("Problem parsing response {}, {}", query, e);
                    return Err(eg!(CliError::Reqwest));
                }

                Ok(v) => {
                    let resp_comm = HashOf::new(&Some(v.state_commitment_data.clone()));
                    let curr_comm = (ledger_state.0).0.clone();
                    if resp_comm != curr_comm {
                        eprintln!(
                            "Server responded with authentication relative to '{}'!",
                            b64enc(&resp_comm.0.hash)
                        );
                        eprintln!(
                            "The most recent ledger state I have is '{}'.",
                            b64enc(&curr_comm.0.hash)
                        );
                        eprintln!(
                            "Please run query-ledger-state then rerun this command."
                        );
                        return Err(eg!(CliError::InconsistentLedger));
                    }

                    // TODO: this needs better direct authentication
                    if v.authenticated_spent_status.utxo_sid != TxoSID(sid) {
                        eprintln!("!!!!! ERROR !!!!!!");
                        eprintln!("The server responded with a different UTXO sid.");
                        eprintln!(
                            "This could indicate a faulty server, or a man-in-the-middle!"
                        );
                        eprintln!("\nFor safety, refusing to update.");
                        return Err(eg!(CliError::InconsistentLedger));
                    }
                    if !v.is_valid((ledger_state.0).0.clone()) {
                        eprintln!("!!!!! ERROR !!!!!!");
                        eprintln!(
                            "The server responded with an invalid authentication proof."
                        );
                        eprintln!(
                            "This could indicate a faulty server, or a man-in-the-middle!"
                        );
                        eprintln!("\nFor safety, refusing to update.");
                        return Err(eg!(CliError::InconsistentLedger));
                    }
                    v
                }
            }
        }
    };
    Ok(resp)
}

#[derive(Debug)]
pub enum PasswordReadError {
    DidNotMatch,
    UserInput,
    IncorrectPassword,
}

impl fmt::Display for PasswordReadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PasswordReadError::DidNotMatch => {
                write!(f, "{}", stringify!(PasswordReadError::DidNotMatch))
            }
            PasswordReadError::UserInput => {
                write!(f, "{}", stringify!(PasswordReadError::UserInput))
            }
            PasswordReadError::IncorrectPassword => {
                write!(f, "{}", stringify!(PasswordReadError::IncorrectPassword))
            }
        }
    }
}

/// Reads a user's mnemonic
///
/// Optionally takes a string describing what the mnemonic is for
pub fn prompt_mnemonic(description: Option<&str>) -> Result<Zeroizing<String>> {
    let prompt = if let Some(s) = description {
        format!("Enter menonic for {}: ", s)
    } else {
        "Enter menonic: ".to_string()
    };

    rpassword::prompt_password_stdout(&prompt)
        .c(d!(PasswordReadError::UserInput))
        .map(Zeroizing::new)
}

/// Reads a user's password without confirming
///
/// Optionally takes a string describing what the password is for
fn prompt_password(description: Option<&str>) -> Result<Zeroizing<String>> {
    let prompt = if let Some(s) = description {
        format!("Enter password for {}: ", s)
    } else {
        "Enter Password: ".to_string()
    };

    rpassword::prompt_password_stdout(&prompt)
        .c(d!(PasswordReadError::UserInput))
        .map(Zeroizing::new)
}

/// Reads a password from the user twice, and confirms that they match
///
/// Optionally takes a string describing what the password is for
fn prompt_password_confirming(description: Option<&str>) -> Result<Zeroizing<String>> {
    let first_prompt = if let Some(s) = description {
        format!("Enter password for {}: ", s)
    } else {
        "Enter Password: ".to_string()
    };

    let first = rpassword::prompt_password_stdout(&first_prompt)
        .c(d!(PasswordReadError::UserInput))
        .map(Zeroizing::new)
        .c(d!())?;

    let second = rpassword::prompt_password_stdout("Enter password again:")
        .c(d!(PasswordReadError::UserInput))
        .map(Zeroizing::new)
        .c(d!())?;
    // Return an error if the entered passwords did not match
    assert_eq!(first, second);

    Ok(first)
}

/// Reads a password from the user twice, and confirms that they match
///
/// Optionally takes a string describing what the password is for
pub fn prompt_confirming_with_retries(
    retries: u32,
    description: Option<&str>,
) -> Result<Zeroizing<String>> {
    let mut ret = None;
    for i in 0..retries {
        let x = prompt_password_confirming(description);
        match x {
            Ok(x) => {
                ret = Some(x);
                break;
            }
            Err(e) => {
                if e.eq_any(eg!(PasswordReadError::DidNotMatch).as_ref()) {
                    if i < retries - 1 {
                        println!("Passwords did not match, please try again.");
                    }
                } else {
                    return Err(eg!(e));
                }
            }
        }
    }
    ret.c(d!(PasswordReadError::DidNotMatch))
}

/// Reads a password, provides it to the provided closure, and will re-attempt if the closure
/// returns an error.
///
/// The provided closure will indicate the provided password is incorrect by returning an 'Err'
/// value. As a matter of correctness, this should be the only condition under which the closure
/// will return an error
pub fn prompt_with_retries<T>(
    retries: u32,
    description: Option<&str>,
    closure: impl Fn(&str) -> Result<T>,
) -> Result<T> {
    for i in 0..retries {
        let password = prompt_password(description).c(d!())?;
        let x = closure(&password);
        match x {
            Ok(x) => return Ok(x),
            Err(_) => {
                if i < retries - 1 {
                    println!("Password was incorrect, please try again.");
                }
            }
        }
    }
    Err(eg!(PasswordReadError::IncorrectPassword))
}

pub fn compute_findora_dir() -> Result<PathBuf> {
    let mut home = PathBuf::new();
    match env::var("FINDORA_HOME") {
        Ok(fin_home) => {
            home.push(fin_home);
        }
        Err(_) => {
            home.push(dirs::home_dir().c(d!(CliError::HomeDir))?);
            home.push(".findora");
        }
    }
    Ok(home)
}
