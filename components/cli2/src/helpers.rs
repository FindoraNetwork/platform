use crate::HomeDir;
use crate::{CliError, LedgerStateCommitment};
use ledger::data_model::{b64enc, Asset, AssetType, AuthenticatedUtxo, TxoSID};
use serde::de::DeserializeOwned;
use snafu::{ensure, Backtrace, GenerateBacktrace, OptionExt, ResultExt, Snafu};
use std::env;
use std::path::PathBuf;
use std::process::exit;
use std::time::Duration;
use structopt::clap::{Error, ErrorKind};
use utils::HashOf;
use zeroize::Zeroizing;

/// Computes a http client with a specific timeout
fn get_client() -> Result<reqwest::blocking::Client, reqwest::Error> {
    const TIMEOUT: u64 = 20;
    let client = reqwest::blocking::Client::builder()
        .timeout(Duration::from_secs(TIMEOUT))
        .build()?;
    Ok(client)
}

pub fn do_request_asset(query: &str) -> Result<Asset, CliError> {
    let client = get_client().map_err(|e| CliError::Reqwest {
        source: e,
        backtrace: Backtrace::generate(),
    })?;

    let resp = match client.get(query).send() {
        Err(e) => {
            eprintln!("Request '{}' failed: {}", query, e);
            return Err(CliError::Reqwest {
                source: e,
                backtrace: Backtrace::generate(),
            });
        }
        Ok(resp) => match resp.json::<AssetType>() {
            Err(e) => {
                eprintln!("Problem parsing response {}, {}", query, e);
                return Err(CliError::Reqwest {
                    source: e,
                    backtrace: Backtrace::generate(),
                });
            }
            Ok(v) => v.properties,
        },
    };

    Ok(resp)
}

pub fn do_request<T: DeserializeOwned>(query: &str) -> Result<T, Error> {
    let client = get_client().map_err(|_| {
        Error::with_description(
            "The http client failed being initialized.",
            ErrorKind::Io,
        )
    })?;

    let resp: T = match client.get(query).send() {
        Err(e) => {
            eprintln!("Request '{}' failed: {}", query, e);
            exit(-1);
        }
        Ok(resp) => match resp.json::<T>() {
            Err(e) => {
                eprintln!("Problem parsing response {}, {}", query, e);
                return Err(Error::with_description(
                    "Problem parsing json",
                    ErrorKind::Io,
                ));
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
) -> Result<AuthenticatedUtxo, CliError> {
    let client = get_client().map_err(|e| CliError::Reqwest {
        source: e,
        backtrace: Backtrace::generate(),
    })?;

    let resp = match client.get(query).send() {
        Err(e) => {
            eprintln!("Request '{}' failed: {}", query, e);
            return Err(CliError::Reqwest {
                source: e,
                backtrace: Backtrace::generate(),
            });
        }
        Ok(resp) => {
            match resp.json::<AuthenticatedUtxo>() {
                Err(e) => {
                    eprintln!("Problem parsing response {}, {}", query, e);
                    return Err(CliError::Reqwest {
                        source: e,
                        backtrace: Backtrace::generate(),
                    });
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
                        return Err(CliError::InconsistentLedger);
                    }

                    // TODO: this needs better direct authentication
                    if v.authenticated_spent_status.utxo_sid != TxoSID(sid) {
                        eprintln!("!!!!! ERROR !!!!!!");
                        eprintln!("The server responded with a different UTXO sid.");
                        eprintln!(
                            "This could indicate a faulty server, or a man-in-the-middle!"
                        );
                        eprintln!("\nFor safety, refusing to update.");
                        return Err(CliError::InconsistentLedger);
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
                        return Err(CliError::InconsistentLedger);
                    }
                    v
                }
            }
        }
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
    #[snafu(display("The provided password was incorrect."))]
    IncorrectPassword,
}

/// Reads a user's password without confirming
///
/// Optionally takes a string describing what the password is for
fn prompt_password(
    description: Option<&str>,
) -> Result<Zeroizing<String>, PasswordReadError> {
    let prompt = if let Some(s) = description {
        format!("Enter password for {}: ", s)
    } else {
        "Enter Password: ".to_string()
    };

    rpassword::prompt_password_stdout(&prompt)
        .context(UserInput)
        .map(Zeroizing::new)
}

/// Reads a password from the user twice, and confirms that they match
///
/// Optionally takes a string describing what the password is for
fn prompt_password_confirming(
    description: Option<&str>,
) -> Result<Zeroizing<String>, PasswordReadError> {
    let first_prompt = if let Some(s) = description {
        format!("Enter password for {}: ", s)
    } else {
        "Enter Password: ".to_string()
    };

    let first = rpassword::prompt_password_stdout(&first_prompt)
        .context(UserInput)
        .map(Zeroizing::new)?;

    let second = rpassword::prompt_password_stdout("Enter password again:")
        .context(UserInput)
        .map(Zeroizing::new)?;
    // Return an error if the entered passwords did not match
    ensure!(first == second, DidNotMatch);
    Ok(first)
}

/// Reads a password from the user twice, and confirms that they match
///
/// Optionally takes a string describing what the password is for
pub fn prompt_confirming_with_retries(
    retries: u32,
    description: Option<&str>,
) -> Result<Zeroizing<String>, PasswordReadError> {
    let mut ret = None;
    for i in 0..retries {
        let x = prompt_password_confirming(description);
        match x {
            Ok(x) => {
                ret = Some(x);
                break;
            }
            Err(e) => {
                if matches!(e,PasswordReadError::DidNotMatch{..}) {
                    if i < retries - 1 {
                        println!("Passwords did not match, please try again.");
                    }
                } else {
                    return Err(e);
                }
            }
        }
    }
    ret.context(DidNotMatch)
}

/// Reads a password, provides it to the provided closure, and will re-attempt if the closure
/// returns an error.
///
/// The provided closure will indicate the provided password is incorrect by returning an 'Err'
/// value. As a matter of correctness, this should be the only condition under which the closure
/// will return an error
pub fn prompt_with_retries<T, E: std::error::Error>(
    retries: u32,
    description: Option<&str>,
    closure: impl Fn(&str) -> Result<T, E>,
) -> Result<T, PasswordReadError> {
    for i in 0..retries {
        let password = prompt_password(description)?;
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
    Err(PasswordReadError::IncorrectPassword)
}

pub fn compute_findora_dir() -> Result<PathBuf, CliError> {
    let mut home = PathBuf::new();
    match env::var("FINDORA_HOME") {
        Ok(fin_home) => {
            home.push(fin_home);
        }
        Err(_) => {
            home.push(dirs::home_dir().context(HomeDir)?);
            home.push(".findora");
        }
    }
    Ok(home)
}
